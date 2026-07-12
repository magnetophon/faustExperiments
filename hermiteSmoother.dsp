declare name "hermiteSmoother";
declare version "0.6";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

//========================================================================
// Lookahead smoother for a limiter: Hermite attack + Hermite release.
// (v0.6: release gated on the big window -- the release ceiling is now
// the big-window min, so the follower holds instead of the old
// release-then-re-attack bump while a deeper point is inbound.
// v0.5: the smoother option checkboxes are gone -- fixed behavior is
// knob release time, flat release landings, no hard safety clamp.
// v0.4: multidetector integrated -- the per-scale sliding-min bank
// guards the attack against intermediate peaks, solving the old first
// limitation. Input is the raw GR signal; the full limiter is
// detector -> this -> apply to delayed audio.)
//
// The smoother is unit-agnostic: it chases whatever the raw GR is (dB,
// linear, or the +-1 test signal); "deepest" = smallest.
//
// --- attack: chase the critical constraint --------------------------
// The pair cascade inside the attack window's sliding min already
// computes every power-of-two block minimum (that is the multidetector's
// sequentialOperatorParOut), so the bank comes for free: fan the cascade
// out to nB aligned taps, each the min over the NEXT 2^i samples
// (trailing edge = the sample playing now), each with the EXACT play
// index of its minimum riding along as a timestamp.
//
// Per sample the follower builds (requiredSlope, value, deadline)
// triples for the exact full-window target (v1, i1) and every active
// tap, and chases the CRITICAL one: steepest required average slope
// (value - gain) / deadline. A new segment is latched when the critical
// value changes, or when its deadline undercuts the running leg's
// remaining time (critDl < T-k; catches an equal-depth peak that plays
// sooner, e.g. on plateaus). Because tap deadlines are exact, a latched
// leg counts down in lockstep with the live deadline: no spurious
// retriggers, and landings stay sample-exact. Checkpoint legs land
// aimed at the final target, (v1-critVal)/(i1-critDl), so intermediate
// touchdowns stay C1 on the way down; the final target lands aimed at a
// deeper point beyond the attack window if the big window (nAtt+nExtra)
// sees one. Taps longer than the current window read ma.MAX and never
// win the argmin.
//
// Why this is safe: a monotone-decreasing curve with p(e_i) <= m_i for
// every scale (m_i = min over next 2^i samples, e_i = its exact play
// time) cannot poke through any peak that is the minimum of some dyadic
// prefix. Residual (honest) gap: a min-summary cannot see a
// "second-deepest" peak shadowed by a deeper-LATER one inside every
// scale that covers it; per-sample criticality closes these in
// practice.
//
// --- release ----------------------------------------------------------
// * The release ceiling is the BIG-window min v2 (deepest value
//   anywhere in the next nAtt+nExtra samples): when it steps UP (the
//   min falls out at the oldest slot) or re-targets between gain and
//   the old target, a release chases it. Up-glides are safe at ANY
//   duration: monotone to v2 stays <= v2 <= grPlay while that min is
//   in the window. T is taste: T = release knob, fixed time per event
//   (chained re-triggers each restart the clock). Segments land flat
//   (scalloped, hugs each ceiling).
// * When v2 undercuts the current gain -- a deeper point is visible in
//   the direction lookahead but not yet inside the attack window -- the
//   effective ceiling is the gain itself: the follower freezes (flat
//   hold, killing any rise in flight) until the point enters the attack
//   window and the attack machinery takes it, instead of releasing and
//   attacking back down. This is what the direction lookahead buys on
//   the release side; at nExtra = 0 (v2 = v1) the release behaves
//   exactly as before.
//
// --- shared machinery -------------------------------------------------
// * Segments start at k = 1: the first step happens on the trigger
//   sample, so per-sample re-triggers degrade into per-sample
//   re-planning instead of freezing, and p(1/T) ~= p0 + m0 keeps the
//   previous velocity through the trigger (C1). Landing is at tau = 1,
//   one sample before the target plays; the target is held through its
//   play sample.
// * Tangents are clamped to the sign-symmetric Fritsch-Carlson monotone
//   box [min(0,3d), max(0,3d)], d = average slope: no bulge past either
//   endpoint. Side effect: an attack fired mid-release flattens a
//   positive start slope, a small corner at the turnaround.
// * Idle (arrived, target unchanged) holds.
//
// Latency: nAtt + nExtra - 1 samples; both lookaheads go up to 50 ms,
// so up to ~100 ms total.
//
// Known limitations, on purpose for now:
// * The shadowed-second-deepest gap described above; with the hard
//   clamp gone it is unbackstopped, so any brickwall violation on the
//   fast S&H stress signal measures exactly this gap.
// * 2 pair-reduce instances (attack window incl. taps, big window).
//========================================================================

//========================================================================
// library part, from slidingMinIdx.dsp v0.1
// (slidingMinIdxBank added)
//========================================================================

//-------------------------`slidingReducePair`--------------------------
// Like ba.slidingReduce, but operating on *pairs* of signals, so a payload
// (here: a timestamp) can ride along with the value being reduced.
//
// #### Usage
//
// ```
// _,_ : slidingReducePair(op,n,maxN,disabledVal1,disabledVal2) : _,_
// ```
//
// Where:
//
// * `op`: 4 inputs -> 2 outputs: (v1,t1,v2,t2) -> (v,t). Needs to be
//   commutative and associative on pairs, and
//   op(v,t,disabledVal1,disabledVal2) must equal (v,t).
//   Selection operators (lexicographic min/max) qualify.
// * `n`: the number of values to process (may vary at runtime, 1 <= n <= maxN)
// * `maxN`: the maximum number of values to process (int, known at compile time, maxN > 0)
// * `disabledVal1`, `disabledVal2`: the pair to use when we want to ignore a value.
//----------------------------------------------------------------------
slidingReducePair(op,n,1,disabledVal1,disabledVal2) = si.bus(2);
slidingReducePair(op,n,maxN,disabledVal1,disabledVal2) =
    sequentialOperatorParOut(maxNrBits(maxN)-1,op)
    : par(i, maxNrBits(maxN),
          (par(j, 2, _@sumOfPrevBlockSizes(i)) : useVal(i)))
    : combinePairs(maxNrBits(maxN))
with {
    sequentialOperatorParOut(N,op) = seq(i, N, operator(i));
    // same as in the mono version, but the running signal is a 2-channel
    // bus, and both channels get delayed by pow2(i)
    operator(i) = si.bus(2*i),
        (si.bus(2) <: (si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))) : op)));
    // ba.parallelOp for pairs; explicit routing instead of partial application
    combinePairs(2) = op;
    combinePairs(N) = (op, si.bus(2*(N-2))) : combinePairs(N-1);
    useVal(i) = select2(isUsed(i), disabledVal1, _),
                select2(isUsed(i), disabledVal2, _);
    // unchanged helpers from ba.slidingReduce:
    // The sum of all the sizes of the previous blocks
    sumOfPrevBlockSizes(0) = 0;
    sumOfPrevBlockSizes(i) = (ba.subseq((allBlockSizes),0,i):>_);
    allBlockSizes = par(i, maxNrBits(maxN-1), (pow2(i)) * isUsed(i));
    maxNrBits(n) = int2nrOfBits(n);
    isUsed(i) = ba.take(i+1, (int2bin(n,(maxN-1)*2+1)));
    pow2(i) = 1<<i;
    int2bin(n,maxN) = par(j, maxNrBits(maxN-1), int(floor((n)/(pow2(j))))%2);
    int2nrOfBits(n) = int(floor(log(n)/log(2))+1);
};

//---------------------------`slidingMinIdx`----------------------------
// The minimum of the last n samples, plus WHERE in the window that
// minimum sits, counted from the oldest sample: 0 = the oldest sample in
// the window, n-1 = the current sample.
//
// In a lookahead context this is "how many samples in the future the new
// minimum occurs": if the through-signal is delayed by n-1 samples, out2
// is the number of samples from "now" until the minimum plays. (With a
// lookahead delay of n samples, add 1.)
//
// If the minimum value occurs more than once in the window, the OLDEST
// occurrence wins, i.e. the one nearest in the future -- equivalently,
// out2 is the number of samples until the current minimum falls out of
// the window. This is the deadline convention: the attack must be done
// by the FIRST time the value plays.
//
// #### Usage
//
// ```
// _ : slidingMinIdx(n,maxN) : _,_
// ```
//
// * out1: the minimum of the last n samples
// * out2: index of that minimum, counted from the oldest sample (0 .. n-1)
//
// To get "how many samples ago" instead, use (n-1) - out2, or output
// ago(tMin) in place of idxFromOldest(tMin).
//
// Notes:
// * the first maxN samples are polluted by the zero-initialized delay
//   lines, just like in ba.slidingMin.
// * timestamps are int32, so ba.time wraps after 2^31 samples
//   (~12.4 h at 48 kHz). The subtraction in ago() and the (t2-t1)<0
//   tie-break are wraparound-safe, so the outputs stay correct across
//   the wrap.
//----------------------------------------------------------------------
slidingMinIdx(n,maxN) =
    (_, ba.time)
    : slidingReducePair(minIdxOp, n, maxN, ma.MAX, intMax)
    : (_, idxFromOldest)
with {
    intMax = 2147483647;
    ago(tMin) = ba.time - tMin;              // samples ago: 0 .. n-1
    idxFromOldest(tMin) = (n-1) - ago(tMin); // samples in the future under n-1 lookahead
    // lexicographic minimum of (value, time) pairs:
    // smaller value wins; on equal values, the older (smaller) timestamp wins
    minIdxOp(v1,t1,v2,t2) =
        select2(pickSecond, v1, v2),
        select2(pickSecond, t1, t2)
    with {
        pickSecond = (v2 < v1) | ((v2 == v1) & ((t2 - t1) < 0));
    };
};

//-------------------------`slidingMinIdxBank`---------------------------
// slidingMinIdx + the multidetector: the pair cascade IS the
// multidetector's sequentialOperatorParOut, so the per-scale taps come
// for free. The cascade is fanned out to:
//   * the full-window (min, idx), verbatim slidingMinIdx semantics
//   * nB = maxNrBits(maxN) taps: tap i = min over the NEXT pow2(i)
//     samples (all taps share their trailing edge with the full window's
//     oldest sample = the one playing now), plus the EXACT play index of
//     that minimum (oldest-occurrence tie-break = deadline convention).
// A tap whose window exceeds the current n outputs (ma.MAX, 1): a value
// that can never become a binding constraint.
//
// #### Usage
//
// ```
// _ : slidingMinIdxBank(n,maxN) : si.bus(2 + 2*maxNrBits(maxN))
// ```
//
// * out1, out2:           full-window min and idx (as slidingMinIdx)
// * out(3+2i), out(4+2i): tap i value and play idx, i = 0 .. nB-1
//----------------------------------------------------------------------
slidingMinIdxBank(n,maxN) =
    (_, ba.time)
    : sequentialOperatorParOut(nB-1)
    <: (fullWin, tapBank)
with {
    nB     = maxNrBits(maxN);
    intMax = 2147483647;
    idxFromOldest(tMin) = (n-1) - (ba.time - tMin);
    minIdxOp(v1,t1,v2,t2) =
        select2(pickSecond, v1, v2),
        select2(pickSecond, t1, t2)
    with {
        pickSecond = (v2 < v1) | ((v2 == v1) & ((t2 - t1) < 0));
    };

    // full-window path, verbatim from slidingReducePair:
    fullWin = par(i, nB, (par(j, 2, _@sumOfPrevBlockSizes(i)) : useVal(i)))
              : combinePairs(nB)
              : (_, idxFromOldest);
    useVal(i) = select2(isUsed(i), ma.MAX, _),
                select2(isUsed(i), intMax, _);

    // tap path: cascade stage i covers [t-pow2(i)+1, t]; delaying the
    // pair by n-pow2(i) moves that to [t-n+1, t-n+pow2(i)]: the first
    // pow2(i) samples to play. The delayed timestamp then yields the
    // exact play index through the same idxFromOldest.
    tapBank = par(i, nB, tap(i));
    tap(i)  = par(j, 2, de.delay(maxN - pow2(i), max(0, n - pow2(i))))
              : (_, idxFromOldest)
              : disable(i);
    disable(i) = select2(active(i), ma.MAX, _),
                 select2(active(i), 1, _);
    active(i) = pow2(i) <= n;

    // shared helpers (op bound to minIdxOp, otherwise verbatim):
    sequentialOperatorParOut(N) = seq(i, N, operator(i));
    operator(i) = si.bus(2*i),
        (si.bus(2) <: (si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))) : minIdxOp)));
    combinePairs(2) = minIdxOp;
    combinePairs(N) = (minIdxOp, si.bus(2*(N-2))) : combinePairs(N-1);
    sumOfPrevBlockSizes(0) = 0;
    sumOfPrevBlockSizes(i) = (ba.subseq((allBlockSizes),0,i):>_);
    allBlockSizes = par(i, maxNrBits(maxN-1), (pow2(i)) * isUsed(i));
    maxNrBits(x) = int2nrOfBits(x);
    isUsed(i) = ba.take(i+1, (int2bin(n,(maxN-1)*2+1)));
    pow2(i) = 1<<i;
    int2bin(x,m) = par(j, maxNrBits(m-1), int(floor((x)/(pow2(j))))%2);
    int2nrOfBits(x) = int(floor(log(x)/log(2))+1);
};

//========================================================================
// smoother
//========================================================================

//---------------------------`hermiteFollower`---------------------------
// Event-driven gain envelope: chases the critical constraint with
// latched Hermite segments.
//
// #### Usage
//
// ```
// hermiteFollower(nB, taps, v1, i1, endDirDn, relCeil, TRel) : _
// ```
//
// Where:
//
// * `nB`: number of taps (compile-time int)
// * `taps`: 2*nB signals: (value, exact play idx) per scale, from
//   slidingMinIdxBank; disabled taps read (ma.MAX, 1)
// * `v1`, `i1`: full-window min and samples until it first plays
// * `endDirDn`: landing slope for the FINAL attack target (<= 0)
// * `relCeil`: release ceiling: the deepest value visible anywhere in
//   the lookahead (the caller's big-window min). Releases rise toward
//   it and never above it; when it undercuts the current gain the
//   follower holds.
// * `TRel`: release segment length in samples
//
// Output: the smoothed gain envelope.
//
// Per sample, every candidate (v1 plus each tap) gets a triple
// (requiredSlope, value, deadline) with requiredSlope =
// (value - gain)/deadline; the critical candidate is the argmin
// (steepest descent required). Candidates at or above gain have
// requiredSlope >= 0 and never win while any descent is needed.
//
// Triggers, mutually exclusive:
// * attack:  critVal < gain, and (critVal != p1 or critDl < T-k)
//            -> T = critDl (exact deadline)
// * release: critVal >= gain & max(gain, relCeil) != p1
//            -> target max(gain, relCeil), T = TRel (taste); lands
//            flat. relCeil < gain thus latches a flat hold: freeze
//            while a deeper point approaches the attack window.
// Idle (arrived, target unchanged) holds.
//----------------------------------------------------------------------
hermiteFollower(nB, taps, v1, i1, endDirDn, relCeil, TRel) =
    (loop ~ si.bus(7)) : (_, si.block(6))
with {
    // state: gain, p0, m0, p1, m1, k, T (previous-sample values inside loop)
    loop(gain, p0, m0, p1, m1, k, T) =
        gainN, p0N, m0N, p1N, m1N, kN, TN
    with {
        dirPrev = gain - gain';        // current slope, units/sample

        // ---- critical-constraint selection ----
        trip(val, dl) = (val - gain) / max(1, dl), val, dl;
        cands   = (v1, i1, taps) : par(i, nB + 1, trip);
        crit    = cands : red3(nB + 1);
        critVal = crit : (!, _, !);
        critDl  = crit : (!, !, _);
        amin3(sa,va,da, sb,vb,db) =
            select2(pk, sa, sb), select2(pk, va, vb), select2(pk, da, db)
        with { pk = sb < sa; };
        red3(1) = si.bus(3);
        red3(2) = amin3;
        red3(N) = (amin3, si.bus(3*(N-2))) : red3(N-1);

        // ---- triggers ----
        attNeed = critVal < gain;
        // re-latch when the critical value changes, or when its (exact)
        // deadline undercuts the running leg's remaining time -- catches
        // an equal-depth peak that plays sooner (plateaus). On a steady
        // leg the live deadline counts down in lockstep with T-k, so
        // this stays quiet.
        attTrig = attNeed & ((critVal != p1) | (critDl < (T - k)));
        // release ceiling: rise toward relCeil, never above it. When
        // relCeil < gain the effective ceiling is the gain itself, so
        // the latched segment is a flat hold (delta = 0 clamps both
        // tangents to 0), freezing any rise in flight. attNeed == 0
        // keeps releases from hijacking a running attack leg.
        effCeil = max(gain, relCeil);
        relTrig = (attNeed == 0) & (effCeil != p1);
        trig    = attTrig | relTrig;

        // ---- new-segment values (only used when trig == 1) ----
        Tt    = max(1, select2(attTrig, TRel, critDl));
        p1t   = select2(attTrig, effCeil, critVal);
        delta = (p1t - gain) / Tt;     // average slope, sign = direction
        // an intermediate checkpoint lands aimed at the final target;
        // the final target lands aimed beyond the window (endDirDn)
        aimDn = select2(critVal > v1,
                        endDirDn,
                        (v1 - critVal) / max(1, i1 - critDl));
        // sign-symmetric Fritsch-Carlson monotone box
        lo    = min(0, 3*delta);
        hi    = max(0, 3*delta);
        m0t   = max(lo, min(hi, dirPrev));
        // releases land flat (end slope 0)
        m1t   = max(lo, min(hi, select2(attTrig, 0, aimDn)));

        TN  = select2(trig, T,  Tt);
        p0N = select2(trig, p0, gain);
        m0N = select2(trig, m0, m0t);
        p1N = select2(trig, p1, p1t);
        m1N = select2(trig, m1, m1t);
        // segments start at k = 1: first step on the trigger sample, so
        // per-sample re-triggers re-plan instead of stalling, and the
        // trigger sample keeps the previous velocity (p(1/T) ~= p0 + m0)
        kN  = select2(trig, min(k + 1, TN + 1), 1);

        // Hermite basis at tau = k/T; lands (tau = 1) one sample before
        // the target plays, then holds it through the play sample.
        tau = kN / max(1, TN);         // int/int division is float in Faust
        t2  = tau * tau;
        t3  = t2 * tau;
        h00 =  2 * t3 - 3 * t2 + 1;
        h10 =      t3 - 2 * t2 + tau;
        h01 = -2 * t3 + 3 * t2;
        h11 =      t3 -     t2;
        hermiteVal = h00 * p0N + h10 * TN * m0N
                   + h01 * p1N + h11 * TN * m1N;

        gliding = kN <= TN;
        gainN   = select2(gliding, gain, hermiteVal);   // idle holds
    };
};

//--------------------------`lookaheadSmoother`--------------------------
// Full smoother wiring: the lookahead windows, the multidetector tap
// bank, the attack end-slope, the release ceiling, and the Hermite
// follower. Input is the RAW GR signal.
//
// #### Usage
//
// ```
// _ : lookaheadSmoother(nAtt, maxAtt, nExtra, maxExtra, relTime) : _
// ```
//
// * `relTime`: release segment length (samples)
//
// Latency: nAtt + nExtra - 1 samples; delay the raw GR (and the audio in
// a full limiter) by the same amount to line up with the output.
//----------------------------------------------------------------------
lookaheadSmoother(nAtt, maxAtt, nExtra, maxExtra, relTime, rawGR) =
    hermiteFollower(nB, taps, v1, i1, dn, v2, relTime)
with {
    nB     = int(floor(log(maxAtt)/log(2)) + 1);
    nTot   = nAtt + nExtra;
    maxTot = maxAtt + maxExtra;
    // the attack window's oldest sample is the one playing now: small
    // covers plays-now .. +(nAtt-1), big covers plays-now .. +(nTot-1)
    grSm   = de.delay(maxExtra, nExtra, rawGR);
    bank   = grSm : slidingMinIdxBank(nAtt, maxAtt);    // v1, i1, taps
    v1     = bank : (_, si.block(1 + 2*nB));
    i1     = bank : (!, _, si.block(2*nB));
    taps   = bank : (si.block(2), si.bus(2*nB));
    big    = rawGR : slidingMinIdx(nTot, maxTot);       // v2, i2
    v2     = big : (_, !);
    i2     = big : (!, _);

    // attack aim: land on v1 moving toward a deeper v2 beyond the attack
    // window. v2 < v1 implies i2 > i1, but guard anyway. v2 doubles as
    // the release ceiling.
    dn = select2(v2 < v1, 0, (v2 - v1) / max(1, i2 - i1));
};

//-------------------------------- demo ---------------------------------
// out1: delayed raw GR (the constraint the smoother must stay <= )
// out2: smoother output
//
// Brickwall check: out2 <= out1 at every sample (no hard clamp, so any
// violation measures the residual shadowed-peak gap).

MainGroup(x) = hgroup("[0]shapedSmoother", x);
TestGroup(x) = vgroup("[0]Test signal", x);
SmootherGroup(x) = vgroup("[1]Smoother", x);

// --- Test signal ---
testNoiseLevel = TestGroup(hslider("[0]noise level", 0, 0, 1, 0.001));
testNoiseRate = TestGroup(hslider("[1]noise rate", 42, 1, 1000, 1));
testBlockscale = TestGroup(hslider("[2]blockscale", 1, 0.01, 10, 0.01));
testFreq = TestGroup(hslider("[3]freq", 1, 0.001, 30, 0.001));
testStep1 = TestGroup(hslider("[4]step1", 0.75, -1, 1, 0.001));
testStep2 = TestGroup(hslider("[5]step2", 0.125, -1, 1, 0.001));
testSelect = TestGroup(checkbox("[6]signal select"));
testSignal = select2(testSelect, testSignal1, testSignal2);
testSignal1 = it.interpolate_linear(testNoiseLevel,
    (loop~_),
    no.lfnoise(testNoiseRate))
with {
    loop(prev) = no.lfnoise0(testBlockscale*(abs(prev*69)%9:pow(0.75)*5+1));
};
testSignal2 = os.lf_squarewave(testFreq)*0.5;

// --- Smoother parameters ---
// compile-time maxima: 50 ms at maxSR. Lower maxSR if you never run
// above 48/96k, to save memory and a few reduce stages.
maxSR    = 192000;
maxAtt   = int(0.05 * maxSR);
maxExtra = int(0.05 * maxSR);
maxTot   = maxAtt + maxExtra;

attMs   = SmootherGroup(hslider("[0]attack lookahead [unit:ms]", 25, 0, 50, 0.1));
extraMs = SmootherGroup(hslider("[1]direction lookahead [unit:ms]", 25, 0, 50, 0.1));
relMs   = SmootherGroup(hslider("[2]release [unit:ms]", 50, 0, 1000, 0.1));

nAtt    = max(2, min(maxAtt,   int(attMs   * 0.001 * ma.SR)));
nExtra  = max(0, min(maxExtra, int(extraMs * 0.001 * ma.SR)));
relTime = max(1, int(relMs * 0.001 * ma.SR));

process = MainGroup(demo(testSignal))
with {
    demo(rawGR) = grPlay, smoothed
    with {
        nTot     = nAtt + nExtra;
        grPlay   = de.delay(maxTot - 1, nTot - 1, rawGR);
        smoothed = lookaheadSmoother(nAtt, maxAtt, nExtra, maxExtra,
                                     relTime, rawGR);
    };
};
