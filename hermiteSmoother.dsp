declare name "hermiteSmoother";
declare version "0.3";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

//========================================================================
// Lookahead smoother for a limiter: Hermite attack + Hermite release.
// (continues hermiteLimiter.dsp v0.2; this version is JUST the smoother:
// input is the raw GR signal, no level detector, no audio path. The full
// limiter is detector -> this -> apply to delayed audio.)
//
// The smoother is unit-agnostic: it chases whatever the raw GR is (dB,
// linear, or the +-1 test signal); "deepest" = smallest.
//
// The output chases v1, the sliding min of the raw GR over the attack
// window. v1 is piecewise constant; every step of v1 triggers a new
// cubic Hermite segment from the current (gain, slope):
//
// * v1 steps DOWN (deeper min enters at the newest slot): attack.
//   Target (v1, land when it first plays: T = i1, oldest occurrence).
//   Hard deadline. End slope aims at a deeper point beyond the attack
//   window if the big window (nAtt + nExtra) sees one.
// * v1 steps UP (min falls out at the oldest slot), or re-targets to a
//   value between gain and the old target: release. Up-glides are safe
//   at ANY duration: monotone to v1 stays <= v1 <= grPlay while that min
//   is in the window. So T is taste, switchable:
//   - knob mode: T = release knob, fixed time per event (chained
//     re-triggers each restart the clock, so a rising staircase moves
//     slower than the knob)
//   - auto mode: T = i1n = samples until the min VALUE leaves the window
//     (newest occurrence): arrive at the ceiling exactly when the
//     ceiling expires. Parameter-free, perpetually in flight.
//   End slope, switchable: flat (scalloped, hugs each ceiling) or aimed
//   at the next ceiling = the min over the samples newer than the newest
//   occurrence of the current min (window w = nAtt-1-i1n; degrades to
//   flat when the min is the newest sample). Aim is gated off when the
//   segment arrives before the ceiling expires, since it would land
//   still rising and then hold.
// * Segments start at k = 1: the first step happens on the trigger
//   sample. Per-sample re-triggers (v1 ramping smoothly on continuous
//   material) then degrade into per-sample re-planning instead of
//   freezing the envelope, and p(1/T) ~= p0 + m0 keeps the previous
//   velocity through the trigger (C1). Landing is at tau = 1, one sample
//   before the target plays; the envelope holds the target while it
//   plays, and the min only falls out one sample after playing, so the
//   release cannot start early.
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
// * A mid-flight attack re-trigger replans toward the newest deepest
//   point only; a shallower minimum that plays *sooner* is not
//   re-checked, so right after a re-trigger the curve can sit slightly
//   above the gain an intermediate peak needs at its play time. The
//   safety clamp (min(gain, grPlay)) restores brickwall at the cost of
//   a corner in exactly that case.
// * After landing on a min while the big window still shows a deeper
//   point that has not entered the attack window yet, the envelope
//   starts releasing and attacks back down when the point arrives (a
//   small bump). Gating the release on the big-window min would hold
//   instead.
// * 4 sliding-reduce instances (attack oldest, attack newest, big, next
//   ceiling); a fused reduce carrying (value, tOldest, tNewest) could
//   cut that down later.
//========================================================================

//========================================================================
// library part, from slidingMinIdx.dsp v0.1
// (slidingMinIdxNewest added; slidingMaxIdx omitted, not needed here)
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

//------------------------`slidingMinIdxNewest`--------------------------
// Same minimum, but on equal values the NEWEST occurrence wins. out2 is
// then the number of samples until the minimum VALUE leaves the window
// (assuming nothing equal re-enters; if it does, the index extends
// automatically). This is the persistence convention the release wants:
// "how long is this the ceiling", where slidingMinIdx answers the attack
// question "when does this first play".
//----------------------------------------------------------------------
slidingMinIdxNewest(n,maxN) =
    (_, ba.time)
    : slidingReducePair(minIdxOp, n, maxN, ma.MAX, intMax)
    : (_, idxFromOldest)
with {
    intMax = 2147483647;
    idxFromOldest(tMin) = (n-1) - (ba.time - tMin);
    // on equal values, the newer (larger) timestamp wins; wraparound-safe
    minIdxOp(v1,t1,v2,t2) =
        select2(pickSecond, v1, v2),
        select2(pickSecond, t1, t2)
    with {
        pickSecond = (v2 < v1) | ((v2 == v1) & ((t2 - t1) > 0));
    };
};

//========================================================================
// smoother
//========================================================================

//---------------------------`hermiteFollower`---------------------------
// Event-driven gain envelope: chases v1 with latched Hermite segments.
//
// #### Usage
//
// ```
// hermiteFollower(v1, i1, endDirDn, endDirUp, TRel, grPlay, clampOn) : _
// ```
//
// Where:
//
// * `v1`, `i1`: slidingMinIdx over the attack window: deepest required
//   gain and samples until it first plays (oldest occurrence)
// * `endDirDn`: landing slope for attacks (units/sample, <= 0)
// * `endDirUp`: landing slope for releases (units/sample, >= 0)
// * `TRel`: release segment length in samples (knob or auto, see caller)
// * `grPlay`: required gain of the sample playing right now
// * `clampOn`: 1 = hard safety clamp min(gain, grPlay)
//
// Output: the smoothed gain envelope.
//
// Triggers, mutually exclusive:
// * attack:  v1 < gain                    -> T = i1 (hard deadline)
// * release: (v1 != p1) & (v1 >= gain)    -> T = TRel (taste)
//   (also catches the ceiling re-targeting DOWN to between gain and the
//   old target: still an up-glide, still safe at any T)
// Idle (arrived, v1 == p1) holds.
//----------------------------------------------------------------------
hermiteFollower(v1, i1, endDirDn, endDirUp, TRel, grPlay, clampOn) =
    (loop ~ si.bus(7)) : (_, si.block(6))
with {
    // state: gain, p0, m0, p1, m1, k, T (previous-sample values inside loop)
    loop(gain, p0, m0, p1, m1, k, T) =
        gainN, p0N, m0N, p1N, m1N, kN, TN
    with {
        dirPrev = gain - gain';        // current slope, units/sample
        attTrig = v1 < gain;
        relTrig = (v1 != p1) & (v1 >= gain);
        trig    = attTrig | relTrig;

        // new-segment values (only used when trig == 1)
        Tt    = max(1, select2(attTrig, TRel, i1));
        delta = (v1 - gain) / Tt;      // average slope, sign = direction
        // sign-symmetric Fritsch-Carlson monotone box
        lo    = min(0, 3*delta);
        hi    = max(0, 3*delta);
        m0t   = max(lo, min(hi, dirPrev));
        m1t   = max(lo, min(hi, select2(attTrig, endDirUp, endDirDn)));

        TN  = select2(trig, T,  Tt);
        p0N = select2(trig, p0, gain);
        m0N = select2(trig, m0, m0t);
        p1N = select2(trig, p1, v1);
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
        gainRaw = select2(gliding, gain, hermiteVal);   // idle holds
        gainN   = select2(clampOn, gainRaw, min(gainRaw, grPlay));
    };
};

//--------------------------`lookaheadSmoother`--------------------------
// Full smoother wiring: the lookahead windows, both end-slopes, release
// timing, and the Hermite follower. Input is the RAW GR signal.
//
// #### Usage
//
// ```
// _ : lookaheadSmoother(nAtt, maxAtt, nExtra, maxExtra,
//                       relTime, autoRel, relAim, clampOn) : _
// ```
//
// * `relTime`: release segment length (samples), used when autoRel == 0
// * `autoRel`: 1 = T = ceiling expiry (newest-occurrence index)
// * `relAim`:  1 = land aimed at the next ceiling, 0 = land flat
//
// Latency: nAtt + nExtra - 1 samples; delay the raw GR (and the audio in
// a full limiter) by the same amount to line up with the output.
//----------------------------------------------------------------------
lookaheadSmoother(nAtt, maxAtt, nExtra, maxExtra, relTime, autoRel, relAim,
                  clampOn, rawGR) =
    (small, i1new, big, grPlay) : wire
with {
    nTot   = nAtt + nExtra;
    maxTot = maxAtt + maxExtra;
    // all attack-window queries share one input; its oldest sample is the
    // one playing now. small covers plays-now .. +(nAtt-1), big covers
    // plays-now .. +(nTot-1)
    grSm   = de.delay(maxExtra, nExtra, rawGR);
    small  = grSm : slidingMinIdx(nAtt, maxAtt);            // v1, i1
    i1new  = grSm : slidingMinIdxNewest(nAtt, maxAtt) : (!, _);
    big    = rawGR : slidingMinIdx(nTot, maxTot);           // v2, i2
    grPlay = de.delay(maxTot - 1, nTot - 1, rawGR);

    wire(v1, i1, i1n, v2, i2, gp) =
        hermiteFollower(v1, i1, dn, up, TRel, gp, clampOn)
    with {
        // attack aim: land on v1 moving toward a deeper v2 beyond the
        // attack window. v2 < v1 implies i2 > i1, but guard anyway.
        dn = select2(v2 < v1, 0, (v2 - v1) / max(1, i2 - i1));
        // next ceiling: min over the samples newer than the newest
        // occurrence of the current min. When the min IS the newest
        // sample, the 1-sample window contains the min itself, so the
        // chord degrades to 0 (flat) on its own.
        wNext = max(1, nAtt - 1 - i1n);
        next  = grSm : slidingMinIdx(wNext, maxAtt);        // vN, iNsub
        chord = next : chordOf;
        // vN plays at full-window index i1n + 1 + iNsub, so the chord
        // from (i1n, v1) to there has run 1 + iNsub (>= 1; guarded for
        // the zero-init pollution period)
        chordOf(vN, iNsub) = (vN - v1) / max(1, 1 + iNsub);
        TRel = select2(autoRel, relTime, i1n);
        // aim only when the segment arrives at/after ceiling expiry;
        // arriving early means holding, so land flat instead
        up   = select2(relAim & (max(1, TRel) >= i1n), 0, chord);
    };
};

//-------------------------------- demo ---------------------------------
// out1: delayed raw GR (the constraint the smoother must stay <= )
// out2: smoother output
//
// Brickwall check: out2 <= out1 at every sample.

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
autoRel = SmootherGroup(checkbox("[3]auto release (T = ceiling expiry)"));
relAim  = SmootherGroup(checkbox("[4]aim release at next ceiling"));
clampOn = SmootherGroup(checkbox("[5]hard safety clamp"));

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
                                     relTime, autoRel, relAim, clampOn, rawGR);
    };
};
