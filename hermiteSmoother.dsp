declare name "hermiteSmoother";
declare version "0.10";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

//========================================================================
// Lookahead smoother for a limiter: Hermite attack + Hermite release.
// (v0.10: the direction lookahead now steers ONLY the end direction:
// it stays the top link of the aim chain -- landings keep aiming at
// the deepest point beyond the attack window -- but the release
// ceiling is back to the attack-window min v1. The v0.6 hold gate is
// gone: a deeper point beyond the attack window no longer freezes the
// follower; the release rises toward v1 and the attack machinery
// takes the point the sample it enters the window. Alignment and
// latency (nAtt + nExtra - 1) are unchanged.
// v0.9: cheaper, bit-identical output. ONE pair cascade over the raw
// signal now feeds everything: a cascade stage delayed by nExtra IS
// the stage of the nExtra-delayed signal, so the big window, the
// attack window and the taps are all just (delay, combine) reads of
// the same stages, and the second reduce instance is gone. Both folds
// (the window combine and the follower's argmin) are balanced trees:
// leftmost-min selection is associative, so the winner is unchanged,
// but the argmin chain sits on the feedback loop's critical path and
// its depth drops from nC-1 to ceil(log2(nC)). Measured on the
// isolated algorithm (harness-fed input, -double, -O3 -march=native,
// 48 kHz): 306 -> 188 ns/sample, 1.6x. Also tried: replacing the
// per-candidate divisions with cross-multiplied comparisons; it
// measured SLOWER (the divisions run in parallel ahead of the select
// chain, the cross-mults sit inside it), so the divide stays.
// v0.8: the next-deeper aim chain moved inside slidingMinIdxBank --
// the bank takes the beyond pair and emits (value, play idx, npV, npD)
// per scale, so lookaheadSmoother is pure wiring. Also fixes a v0.7
// bug: sV(i) = min(tV(i), v1) always returned v1, because tap windows
// nest inside the full window so every tap min is >= v1; the whole
// chain collapsed onto the big-window link and every landing aimed
// there. The per-scale neighbour aim only now takes effect.
// v0.7: unified aim -- every candidate carries the nearest strictly
// deeper point one scale out, from the neighbour tap in the bank;
// checkpoint touchdowns aim there instead of at the final target, and
// the big window is the top link of the same chain.
// v0.6: release gated on the big window -- the release ceiling is now
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
// The pair cascade under the big window's sliding min already computes
// every power-of-two block minimum (that is the multidetector's
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
// retriggers, and landings stay sample-exact. Every landing aims at
// the nearest strictly-deeper point one scale out: a checkpoint at its
// neighbour tap's min (passing outward through scales that share the
// same min), the final target at the big window's min beyond the
// attack window (nAtt+nExtra) -- one uniform chain, from pair data the
// cascade already computes. Taps longer than the current window read
// ma.MAX and never win the argmin.
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
// * The release ceiling is the ATTACK-window min v1 (deepest value in
//   the next nAtt samples): when it steps UP (the min falls out at the
//   oldest slot) or re-targets between gain and the old target, a
//   release chases it. Up-glides are safe at ANY duration: monotone to
//   v1 stays <= v1 <= grPlay while that min is in the window. T is
//   taste: T = release knob, fixed time per event (chained re-triggers
//   each restart the clock). Segments land flat (scalloped, hugs each
//   ceiling).
// * The direction lookahead plays no part here (since v0.10): a deeper
//   point beyond the attack window no longer holds the release down.
//   The follower releases toward v1 and re-attacks when the point
//   crosses into the attack window (C1 through the trigger; the FC box
//   flattens the positive start slope, the usual small corner at the
//   turnaround). nExtra only steers landing directions, via the aim
//   chain's top link.
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
// * The shared cascade buys its ops with state: the attack-window and
//   tap reads sit up to nExtra deeper in the shared delay lines, so
//   the buffers grow vs v0.8 (2.4 -> 3.9 MB at maxSR = 192k in double,
//   0.5 -> 0.9 MB at maxSR = 48k). Lower maxSR if you never run 192k.
//========================================================================

//========================================================================
// library part, from slidingMinIdx.dsp v0.1
// (slidingMinIdxBank added; since v0.8 the aim chain lives inside;
// since v0.9 the bank runs ONE shared cascade over the raw signal,
// computes the big window itself and returns it first.
// slidingReducePair and slidingMinIdx are unused here since v0.9 --
// kept as the reference implementations the bank's semantics are
// defined against.)
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
slidingReducePair(op, n, 1, disabledVal1, disabledVal2) = si.bus(2);
slidingReducePair(op, n, maxN, disabledVal1, disabledVal2) = sequentialOperatorParOut(maxNrBits(maxN)-1, op):par(i, maxNrBits(maxN), (par(j, 2, _@sumOfPrevBlockSizes(i)):useVal(i))):combinePairs(maxNrBits(maxN))
    with {
        sequentialOperatorParOut(N, op) = seq(i, N, operator(i));
        // same as in the mono version, but the running signal is a 2-channel
        // bus, and both channels get delayed by pow2(i)
        operator(i) = si.bus(2*i), (si.bus(2)<:(si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))):op)));
        // ba.parallelOp for pairs; explicit routing instead of partial application
        combinePairs(2) = op;
        combinePairs(N) = (op, si.bus(2*(N-2))):combinePairs(N-1);
        useVal(i) = select2(isUsed(i), disabledVal1, _), select2(isUsed(i), disabledVal2, _);
        // unchanged helpers from ba.slidingReduce:
        // The sum of all the sizes of the previous blocks
        sumOfPrevBlockSizes(0) = 0;
        sumOfPrevBlockSizes(i) = (ba.subseq((allBlockSizes), 0, i):>_);
        allBlockSizes = par(i, maxNrBits(maxN-1), (pow2(i))*isUsed(i));
        maxNrBits(n) = int2nrOfBits(n);
        isUsed(i) = ba.take(i+1, (int2bin(n, (maxN-1)*2+1)));
        pow2(i) = 1<<i;
        int2bin(n, maxN) = par(j, maxNrBits(maxN-1), int(floor((n)/(pow2(j))))%2);
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
slidingMinIdx(n, maxN) = (_, ba.time):slidingReducePair(minIdxOp, n, maxN, ma.MAX, intMax):(_, idxFromOldest)
    with {
        intMax = 2147483647;
        ago(tMin) = ba.time-tMin;
        // samples ago: 0 .. n-1
        idxFromOldest(tMin) = (n-1)-ago(tMin);
        // samples in the future under n-1 lookahead
        // lexicographic minimum of (value, time) pairs:
        // smaller value wins; on equal values, the older (smaller) timestamp wins
        minIdxOp(v1, t1, v2, t2) = select2(pickSecond, v1, v2), select2(pickSecond, t1, t2)
            with {
                pickSecond = (v2<v1)|((v2==v1)&((t2-t1)<0));
            };
    };

//-------------------------`slidingMinIdxBank`---------------------------
// slidingMinIdx + the multidetector + the aim chain, all off ONE pair
// cascade (since v0.9): the cascade over the RAW signal already
// contains every dyadic (min, oldest timestamp) pair the big window
// needs, and delaying a cascade stage by nExtra is the same as running
// the cascade on the nExtra-delayed signal -- so the big window, the
// attack window and the taps are all just different (delay, combine)
// reads of the same nBT stage pairs. One reduce instance instead of
// two; the attack-side reads pay for it by sitting nExtra deeper in
// the (shared) delay lines.
//
// Scales, small to large: tap i = min over the NEXT pow2(i) samples
// (all taps share their trailing edge with the attack window's oldest
// sample = the one playing now), i = 0 .. nB-1; above them the attack
// window over nAtt (verbatim slidingMinIdx semantics), and on top the
// big window over nAtt+nExtra. Every min rides with the EXACT play
// index of its oldest occurrence (deadline convention).
//
// The next-deeper chain: np(scale) = the neighbour one scale out when
// its min is STRICTLY deeper, else that neighbour's own np. Nested
// dyadic windows share mins constantly, and under the oldest-occurrence
// tie-break an equal neighbour is the SAME point (the bigger window
// only adds newer samples), so passing outward through equal scales is
// what keeps intermediate touchdowns from landing flat mid-descent.
// The chain tops out at the big-window pair (v2, i2), computed in
// here; when it is not strictly deeper than v1 the top link degrades
// to the flat sentinel (v1, i1+1), i.e. a zero chord. A strictly
// deeper point always lies outside the inner window, so np play
// indices exceed the inner ones and the chords stay well-formed.
//
// A tap whose window exceeds the current nAtt outputs (ma.MAX, 1): a
// value that can never become a binding constraint. Its window clipped
// to nAtt IS the attack window, so it enters the chain as (v1, i1),
// and equal values pass outward through it like any shared min.
//
// #### Usage
//
// ```
// _ : slidingMinIdxBank(nAtt,maxAtt,nExtra,maxExtra) : si.bus(1+4*(nB+1))
// ```
//
// * input: the RAW GR signal (undelayed; the bank aligns the attack
//   window nExtra samples back internally)
// * `nAtt`: attack window length (1 <= nAtt <= maxAtt, may vary)
// * `nExtra`: direction lookahead beyond it (clamped to 0 .. maxExtra)
// * `maxAtt`, `maxExtra`: compile-time maxima (ints)
// * out1:                  v2, the big-window min over the next
//                          nAtt+nExtra samples (the aim chain's top
//                          link; unused by the caller since v0.10)
// * out2..out5:            v1, i1, npFullV, npFullD (attack window)
// * out(6+4i)..out(9+4i):  tap i value, play idx, npV, npD
//----------------------------------------------------------------------
slidingMinIdxBank(nAtt, maxAtt, nExtra, maxExtra, x) = v2, (v1, i1, npFullV, npFullD), par(i, nB, (outV(i), outD(i), npTV(i), npTD(i)))
    with {
        nE = min(maxExtra, max(0, nExtra));
        nTot = nAtt+nE;
        maxTot = maxAtt+maxExtra;
        nB = maxNrBits(maxAtt);
        // tap scales 2^0 .. 2^(nB-1)
        nBT = maxNrBits(maxTot);
        // cascade stages / big-window blocks
        intMax = 2147483647;
        // one index space for everything: timestamps are raw-signal time,
        // play idx 0 = the sample playing now = the big window's oldest
        idxFromOldest(tMin) = (nTot-1)-(ba.time-tMin);
        minIdxOp(va, ta, vb, tb) = select2(pickSecond, va, vb), select2(pickSecond, ta, tb)
            with {
                pickSecond = (vb<va)|((vb==va)&((tb-ta)<0));
            };

        // THE shared cascade: pair i = (min, oldest timestamp) over the
        // last pow2(i) raw input samples
        casc = (x, ba.time):sequentialOperatorParOut(nBT-1);
        cV(i) = casc:ba.selector(2*i, 2*nBT);
        cT(i) = casc:ba.selector(2*i+1, 2*nBT);

        // a sliding min+idx over the last m samples of x@extraDel, read off
        // the shared stages: block i = stage i delayed into place, disabled
        // blocks read the identity pair (verbatim slidingReducePair, plus
        // extraDel on every block delay, and the fold is a balanced tree)
        window(m, mMax, extraDel, nBl) = par(i, nBl, ((cV(i), cT(i)):par(j, 2, _@(extraDel+sumPrevBlocks(m, mMax, i))):useVal(m, mMax, i))):combineTree(nBl);
        useVal(m, mMax, i) = select2(isUsed(m, mMax, i), ma.MAX, _), select2(isUsed(m, mMax, i), intMax, _);

        // attack window: the last nAtt samples of x@nExtra (oldest = the
        // sample playing now); big window: the last nAtt+nExtra samples of x
        fullA = window(nAtt, maxAtt, nE, nB);
        fullT = window(nTot, maxTot, 0, nBT);
        v1 = fullA:(_, !);
        i1 = fullA:(!, _):idxFromOldest;
        v2 = fullT:(_, !);
        i2 = fullT:(!, _):idxFromOldest;

        // tap path: cascade stage i covers [t-pow2(i)+1, t]; delaying the
        // pair by nTot-pow2(i) moves that to the first pow2(i) samples to
        // play. The delayed timestamp then yields the exact play index
        // through the same idxFromOldest.
        dl(i) = de.delay(maxTot-pow2(i), max(0, nTot-pow2(i)));
        tV(i) = cV(i):dl(i);
        tD(i) = cT(i):dl(i):idxFromOldest;
        active(i) = pow2(i)<=nAtt;
        outV(i) = select2(active(i), ma.MAX, tV(i));
        outD(i) = select2(active(i), 1, tD(i));

        // --- the next-deeper chain --- (unchanged; the beyond pair is now
        // the in-house big window)
        // sV/sD: the tap clipped to the current window; a disabled tap's
        // clipped window IS the attack window
        sV(i) = select2(active(i), v1, tV(i));
        sD(i) = select2(active(i), i1, tD(i));
        // top link: the big-window pair when strictly deeper, else the
        // flat sentinel
        npFullV = select2(v2<v1, v1, v2);
        npFullD = select2(v2<v1, i1+1, i2);
        // chain indexed from the top: k = 0 is the largest tap, whose
        // neighbour is the attack window; tap i sits at k = nB-1-i
        npKV(0) = select2(v1<sV(nB-1), npFullV, v1);
        npKD(0) = select2(v1<sV(nB-1), npFullD, i1);
        npKV(k) = select2(sV(nB-k)<sV(nB-1-k), npKV(k-1), sV(nB-k));
        npKD(k) = select2(sV(nB-k)<sV(nB-1-k), npKD(k-1), sD(nB-k));
        npTV(i) = npKV(nB-1-i);
        npTD(i) = npKD(nB-1-i);

        // shared helpers (op bound to minIdxOp; the pair fold is a balanced
        // tree -- min with leftmost tie-break is associative, so the result
        // is bit-identical to the sequential fold, just a shorter
        // dependency chain):
        sequentialOperatorParOut(N) = seq(i, N, operator(i));
        operator(i) = si.bus(2*i), (si.bus(2)<:(si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))):minIdxOp)));
        combineTree(1) = si.bus(2);
        combineTree(2) = minIdxOp;
        combineTree(N) = (combineTree(half), combineTree(N-half)):minIdxOp
            with {
                half = int(N/2);
            };
        isUsed(m, mMax, i) = ba.take(i+1, (int2bin(m, (mMax-1)*2+1)));
        sumPrevBlocks(m, mMax, 0) = 0;
        sumPrevBlocks(m, mMax, i) = (ba.subseq((allBlockSizes(m, mMax)), 0, i):>_);
        allBlockSizes(m, mMax) = par(j, maxNrBits(mMax-1), (pow2(j))*isUsed(m, mMax, j));
        maxNrBits(m) = int2nrOfBits(m);
        pow2(i) = 1<<i;
        int2bin(v, m) = par(j, maxNrBits(m-1), int(floor((v)/(pow2(j))))%2);
        int2nrOfBits(v) = int(floor(log(v)/log(2))+1);
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
// hermiteFollower(nC, cands, relCeil, TRel) : _
// ```
//
// Where:
//
// * `nC`: number of candidates (compile-time int)
// * `cands`: 4*nC signals: (value, deadline, next-deeper value,
//   next-deeper deadline) per candidate. The deadline is the exact
//   play index; the next-deeper pair is the nearest strictly-deeper
//   point one scale out and becomes the landing chord (pass the own
//   value as next-deeper value to land flat). Disabled candidates read
//   (ma.MAX, 1, _, _) and never win.
// * `relCeil`: release ceiling. Releases rise toward it and never
//   above it; should it undercut the current gain the follower holds
//   flat. Since v0.10 the caller passes the attack-window min v1,
//   which can't undercut the gain while no attack is needed, so the
//   hold branch is dormant; pass the big-window min v2 instead to get
//   the v0.6..v0.9 hold-while-inbound behavior back.
// * `TRel`: release segment length in samples
//
// Output: the smoothed gain envelope.
//
// Per sample, every candidate gets (requiredSlope, value, deadline,
// npVal, npDl) with requiredSlope = (value - gain)/deadline; the
// critical candidate is the argmin (steepest descent required), and
// its chord (npVal - value)/(npDl - deadline) is the landing slope.
// Candidates at or above gain have requiredSlope >= 0 and never win
// while any descent is needed.
//
// Triggers, mutually exclusive:
// * attack:  critVal < gain, and (critVal != p1 or critDl < T-k)
//            -> T = critDl (exact deadline)
// * release: critVal >= gain & max(gain, relCeil) != p1
//            -> target max(gain, relCeil), T = TRel (taste); lands
//            flat. relCeil < gain would latch a flat hold (dormant
//            under the v1 wiring, see above).
// Idle (arrived, target unchanged) holds.
//----------------------------------------------------------------------
hermiteFollower(nC, cands, relCeil, TRel) = (loop~si.bus(7)):(_, si.block(6))
    with {
        // state: gain, p0, m0, p1, m1, k, T (previous-sample values inside loop)
        loop(gain, p0, m0, p1, m1, k, T) = gainN, p0N, m0N, p1N, m1N, kN, TN
            with {
                dirPrev = gain-gain';
                // current slope, units/sample

                // ---- critical-constraint selection ----
                trip(val, dl, npv, npd) = (val-gain)/max(1, dl), val, dl, npv, npd;
                scored = cands:par(i, nC, trip);
                crit = scored:red5(nC);
                critVal = crit:(!, _, !, !, !);
                critDl = crit:(!, !, _, !, !);
                critNpV = crit:(!, !, !, _, !);
                critNpD = crit:(!, !, !, !, _);
                amin5(sa, va, da, ua, wa, sb, vb, db, ub, wb) = select2(pk, sa, sb), select2(pk, va, vb), select2(pk, da, db), select2(pk, ua, ub), select2(pk, wa, wb)
                    with {
                        pk = sb<sa;
                    };
                // balanced tree: leftmost-min selection is associative, so the
                // winner matches the sequential fold, with a dependency chain
                // of ceil(log2(nC)) instead of nC-1 selects -- and this chain
                // sits on the feedback loop's critical path (the divisions all
                // run in parallel ahead of it).
                red5(1) = si.bus(5);
                red5(2) = amin5;
                red5(N) = (red5(half), red5(N-half)):amin5
                    with {
                        half = int(N/2);
                    };

                // ---- triggers ----
                attNeed = critVal<gain;
                // re-latch when the critical value changes, or when its (exact)
                // deadline undercuts the running leg's remaining time -- catches
                // an equal-depth peak that plays sooner (plateaus). On a steady
                // leg the live deadline counts down in lockstep with T-k, so
                // this stays quiet.
                attTrig = attNeed&((critVal!=p1)|(critDl<(T-k)));
                // release ceiling: rise toward relCeil, never above it. When
                // relCeil < gain the effective ceiling is the gain itself, so
                // the latched segment is a flat hold (delta = 0 clamps both
                // tangents to 0), freezing any rise in flight. attNeed == 0
                // keeps releases from hijacking a running attack leg.
                effCeil = max(gain, relCeil);
                relTrig = (attNeed==0)&(effCeil!=p1);
                trig = attTrig|relTrig;

                // ---- new-segment values (only used when trig == 1) ----
                Tt = max(1, select2(attTrig, TRel, critDl));
                p1t = select2(attTrig, effCeil, critVal);
                delta = (p1t-gain)/Tt;
                // average slope, sign = direction
                // every landing aims at the nearest strictly-deeper point one
                // scale out (the critical candidate's np pair); the chord is 0
                // when nothing deeper is in sight (sentinel: npVal = own value)
                aimDn = (critNpV-critVal)/max(1, critNpD-critDl);
                // sign-symmetric Fritsch-Carlson monotone box
                lo = min(0, 3*delta);
                hi = max(0, 3*delta);
                m0t = max(lo, min(hi, dirPrev));
                // releases land flat (end slope 0)
                m1t = max(lo, min(hi, select2(attTrig, 0, aimDn)));

                TN = select2(trig, T, Tt);
                p0N = select2(trig, p0, gain);
                m0N = select2(trig, m0, m0t);
                p1N = select2(trig, p1, p1t);
                m1N = select2(trig, m1, m1t);
                // segments start at k = 1: first step on the trigger sample, so
                // per-sample re-triggers re-plan instead of stalling, and the
                // trigger sample keeps the previous velocity (p(1/T) ~= p0 + m0)
                kN = select2(trig, min(k+1, TN+1), 1);

                // Hermite basis at tau = k/T; lands (tau = 1) one sample before
                // the target plays, then holds it through the play sample.
                tau = kN/max(1, TN);
                // int/int division is float in Faust
                t2 = tau*tau;
                t3 = t2*tau;
                h00 = 2*t3-3*t2+1;
                h10 = t3-2*t2+tau;
                h01 = -2*t3+3*t2;
                h11 = t3-t2;
                hermiteVal = h00*p0N+h10*TN*m0N+h01*p1N+h11*TN*m1N;

                gliding = kN<=TN;
                gainN = select2(gliding, gain, hermiteVal);
                // idle holds
            };
    };

//--------------------------`lookaheadSmoother`--------------------------
// Full smoother wiring: the bank (one shared cascade: both windows,
// the taps, the next-deeper aim chain) and the Hermite follower.
// Input is the RAW GR signal.
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
lookaheadSmoother(nAtt, maxAtt, nExtra, maxExtra, relTime, rawGR) = hermiteFollower(nB+1, cands, v1, relTime)
    with {
        nB = int(floor(log(maxAtt)/log(2))+1);
        // the bank returns the big-window min v2 first (cut: since v0.10
        // the direction lookahead only steers the aim chain, inside the
        // bank), then the follower's candidate list: (value, deadline,
        // npV, npD) for the attack window, then for every tap. The
        // release ceiling is v1, the candidate list's first signal.
        bank = rawGR:slidingMinIdxBank(nAtt, maxAtt, nExtra, maxExtra);
        v1 = bank:(!, _, si.block(4*(nB+1)-1));
        cands = bank:(!, si.bus(4*(nB+1)));
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
maxSR = 192000;
maxAtt = int(0.05*maxSR);
maxExtra = int(0.05*maxSR);
maxTot = maxAtt+maxExtra;

attMs = SmootherGroup(hslider("[0]attack lookahead [unit:ms]", 25, 0, 50, 0.1));
extraMs = SmootherGroup(hslider("[1]direction lookahead [unit:ms]", 25, 0, 50, 0.1));
relMs = SmootherGroup(hslider("[2]release [unit:ms]", 50, 0, 1000, 0.1));

nAtt = max(2, min(maxAtt, int(attMs*0.001*ma.SR)));
nExtra = max(0, min(maxExtra, int(extraMs*0.001*ma.SR)));
relTime = max(1, int(relMs*0.001*ma.SR));

process = MainGroup(demo(testSignal))
    with {
        demo(rawGR) = grPlay, smoothed, grPlay<smoothed, grPlay==smoothed
            with {
                nTot = nAtt+nExtra;
                grPlay = de.delay(maxTot-1, nTot-1, rawGR);
                smoothed = lookaheadSmoother(nAtt,
                    maxAtt,
                    nExtra,
                    maxExtra,
                    relTime,
                    rawGR);
            };
    };
