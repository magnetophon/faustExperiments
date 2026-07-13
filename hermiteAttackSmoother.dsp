declare name "hermiteAttackSmoother";
declare version "0.3";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

//========================================================================
// Attack-only lookahead smoother: hermiteSmoother v0.9 with the release
// machinery deleted at COMPILE time. The attack side is unchanged: one
// shared pair cascade, the per-scale tap bank with exact deadlines, the
// next-deeper aim chain, and the critical-constraint Hermite follower.
// Gone: the direction lookahead (nExtra), the big window, the release
// ceiling, release segments, and the freeze.
//
// When no descent is pending the output TRACKS the sample now playing
// (instant release). This is safe: idle means every candidate value is
// >= gain, so v1 >= gain, and the playing sample is inside the window,
// so ceilPlay >= v1 >= gain -- tracking never jumps down, and
// out <= grPlay holds by the same argument as v0.9. On the sample
// after a landing the target itself is playing, so ceilPlay = p1 and
// the target is still held through its play sample.
//
// Release is the caller's job now: one-pole the raw GR's rises BEFORE
// this stage (the classic detector -> release -> lookahead-attack
// split; the tracked rises are then as smooth as the input), or smooth
// the rises downstream. This stage only shapes descents.
//
// Why delete instead of turning the direction knob to 0: Faust has no
// runtime branching, so extraMs = 0 in v0.9 still runs the big
// window's block reads, its combine tree and the release selects every
// sample -- the isUsed gates are slider-derived, so nothing folds.
// Removing the machinery at compile time is what pays. Measured on
// the isolated algorithm (harness-fed S&H input, -double, -O3
// -march=native, 48 kHz, defaults, maxSR = 192k): v0.9 133 ns/sample
// (136 with the direction knob at 0 -- the knob saves nothing),
// attack-only 115 ns/sample, ~13% less; the follower's per-candidate
// divisions and argmin tree dominate and are unchanged. The bigger
// win is state: sizeof(dsp) 5.6 MB -> 2.4 MB (2.3x), since every
// shared-cascade read sits nExtra shallower and the big-window and
// nExtra-deep tap lines are gone. Attack legs verified bit-identical
// to v0.9 launched from equal idle states (square wave, double).
//
// Semantics changes vs v0.9, attack side:
// * The aim chain's top link is the flat sentinel (v1, i1+1)
//   unconditionally: nothing is visible beyond the attack window, so
//   landings on the full-window target land flat. Checkpoint
//   touchdowns INSIDE the window still aim at the next-deeper tap
//   exactly as before.
// * The trigger gains an `arrived` term: idle tracking moves gain off
//   the latched p1, so critVal == p1 no longer implies the leg is
//   already handled; any needed descent from idle latches a new leg.
//   (In v0.9 idle held gain == p1, which made the != test sufficient.)
// * (v0.2) Launches from idle pick up the direction of the raw GR the
//   follower was tracking -- including a RISING one -- as the leg's
//   launch tangent, so the gain curve leaves the constraint at the
//   constraint's own velocity (C1) instead of cornering flat. With an
//   upstream release on the raw GR this is the normal case: every
//   attack starts on a rising slope. The rising tangent is capped by
//   the mirrored Fritsch-Carlson bound (|m0| <= 3*|delta|), which
//   trades leg monotonicity for continuity; brickwall safety never
//   rested on monotonicity but on the per-sample deadline re-trigger,
//   which re-plans the moment any candidate outruns the running leg.
//   Mid-leg re-latches kept the one-sided v0.9 clamp (launch flat or
//   downward), so a re-plan during the pickup bump could not chain
//   bumps.
// * (v0.3) The v0.2 pickup only survived on drops that were already
//   deep at first sight AND landed on a local minimum; instrumented
//   renders showed every other launch cornering flat through two
//   holes. (1) Whenever the raw GR keeps descending after the drop --
//   a staircase, or merely the sub-sample creep of real material --
//   each newest window sample is a fresh minimum, so critVal != p1
//   re-latches EVERY sample and the one-sided clamp zeroed the
//   tangent one sample after a perfect pickup. (2) When the
//   constraint rises PAST a shallow future value, the trigger fires
//   with gain - critVal at one rise-step, so the mirrored cap
//   3*|delta| was ~0 exactly when the pickup should engage. Both fall
//   to m0t = max(lo, dirPrev): re-latches are velocity-continuous in
//   both directions, so the re-latch storm re-plans one smooth hump
//   instead of killing it, and the rising side is uncapped -- dirPrev
//   is the constraint's own tracked velocity, so the hump peaks at
//   most (4/27)*T*dirPrev above the departure point, a fraction of
//   the rise the constraint itself was making. Humps cannot chain:
//   with m1 >= lo the launch acceleration is <= -4*m0/T whenever
//   m0 > 0, so per-sample re-plans decay the velocity geometrically
//   (time constant T/4 samples) and the hump rolls over on its own.
//   Trade-off: the hump decays with time constant T/4, so when the
//   tracked RISE decelerates faster than that (fast upstream
//   release, or the release's one-pole target moving closer) the
//   hump can poke above the rise briefly -- never above a peak
//   deadline, those re-trigger. Measured on the demo torture signal
//   (10 s, rel 50 ms, att 50 ms, 48 kHz, -double): 5 events,
//   <= 2.3e-4 linear, longest 1.9 ms; v0.2 measured exactly 0. If
//   out <= grPlay must be bit-exact, min the output with ceilPlay,
//   at the price of a C1 corner at the touch point.
//
// Latency: nAtt - 1 samples.
//
// Known limitations, on purpose:
// * The shadowed-second-deepest gap of v0.9 is unchanged (a min
//   summary cannot see a second-deepest peak shadowed by a
//   deeper-LATER one inside every scale that covers it).
// * No release means rises are steps up to the playing constraint;
//   unsuitable on its own as a limiter gain -- pair it with a release
//   stage as described above.
//========================================================================

//========================================================================
// library part, from hermiteSmoother.dsp v0.9
// (slidingMinIdxBankAtt = slidingMinIdxBank minus the big window; the
// reference implementations slidingReducePair / slidingMinIdx that the
// bank's semantics are defined against live in hermiteSmoother.dsp.)
//========================================================================

//-------------------------`slidingMinIdxBankAtt`------------------------
// The v0.9 bank minus the big window: ONE pair cascade over the raw
// signal; the attack window and the taps are (delay, combine) reads of
// its stages. nExtra is gone, so every read sits at its v0.8 depth
// again (extraDel = 0 everywhere) and the cascade needs only nB stages
// instead of nBT.
//
// Scales, small to large: tap i = min over the NEXT pow2(i) samples
// (all taps share their trailing edge with the attack window's oldest
// sample = the one playing now), i = 0 .. nB-1; on top the attack
// window over nAtt (verbatim slidingMinIdx semantics). Every min rides
// with the EXACT play index of its oldest occurrence (deadline
// convention).
//
// The next-deeper chain is as in v0.9, but its top link is the flat
// sentinel (v1, i1+1) unconditionally: with no lookahead beyond the
// window there is never a visible strictly-deeper point outside it, so
// landings on the full-window target land flat (zero chord).
//
// A tap whose window exceeds the current nAtt outputs (ma.MAX, 1): a
// value that can never become a binding constraint. Its window clipped
// to nAtt IS the attack window, so it enters the chain as (v1, i1),
// and equal values pass outward through it like any shared min.
//
// #### Usage
//
// ```
// _ : slidingMinIdxBankAtt(nAtt,maxAtt) : si.bus(4*(nB+1))
// ```
//
// * input: the RAW GR signal
// * `nAtt`: attack window length (1 <= nAtt <= maxAtt, may vary)
// * `maxAtt`: compile-time maximum (int)
// * out1..out4:            v1, i1, npFullV, npFullD (attack window)
// * out(5+4i)..out(8+4i):  tap i value, play idx, npV, npD
//----------------------------------------------------------------------
slidingMinIdxBankAtt(nAtt,maxAtt,x) =
    (v1, i1, npFullV, npFullD),
    par(i, nB, (outV(i), outD(i), npTV(i), npTD(i)))
with {
    nB     = maxNrBits(maxAtt);   // tap scales 2^0 .. 2^(nB-1)
    intMax = 2147483647;
    // play idx 0 = the sample playing now = the attack window's oldest
    idxFromOldest(tMin) = (nAtt-1) - (ba.time - tMin);
    minIdxOp(va,ta,vb,tb) =
        select2(pickSecond, va, vb),
        select2(pickSecond, ta, tb)
    with {
        pickSecond = (vb < va) | ((vb == va) & ((tb - ta) < 0));
    };

    // THE shared cascade: pair i = (min, oldest timestamp) over the
    // last pow2(i) raw input samples
    casc  = (x, ba.time) : sequentialOperatorParOut(nB-1);
    cV(i) = casc : ba.selector(2*i,     2*nB);
    cT(i) = casc : ba.selector(2*i + 1, 2*nB);

    // a sliding min+idx over the last m samples of x, read off the
    // shared stages: block i = stage i delayed into place, disabled
    // blocks read the identity pair; the fold is a balanced tree
    window(m, mMax, nBl) =
        par(i, nBl, ((cV(i), cT(i))
                     : par(j, 2, _@sumPrevBlocks(m, mMax, i))
                     : useVal(m, mMax, i)))
        : combineTree(nBl);
    useVal(m, mMax, i) = select2(isUsed(m, mMax, i), ma.MAX, _),
                         select2(isUsed(m, mMax, i), intMax, _);

    // attack window: the last nAtt samples of x (oldest = the sample
    // playing now)
    fullA = window(nAtt, maxAtt, nB);
    v1 = fullA : (_, !);
    i1 = fullA : (!, _) : idxFromOldest;

    // tap path: cascade stage i covers [t-pow2(i)+1, t]; delaying the
    // pair by nAtt-pow2(i) moves that to the first pow2(i) samples to
    // play. The delayed timestamp then yields the exact play index
    // through the same idxFromOldest.
    dl(i)  = de.delay(maxAtt - pow2(i), max(0, nAtt - pow2(i)));
    tV(i)  = cV(i) : dl(i);
    tD(i)  = cT(i) : dl(i) : idxFromOldest;
    active(i) = pow2(i) <= nAtt;
    outV(i) = select2(active(i), ma.MAX, tV(i));
    outD(i) = select2(active(i), 1,      tD(i));

    // --- the next-deeper chain --- (as v0.9; the top link degrades to
    // the flat sentinel: no lookahead beyond the window)
    // sV/sD: the tap clipped to the current window; a disabled tap's
    // clipped window IS the attack window
    sV(i) = select2(active(i), v1, tV(i));
    sD(i) = select2(active(i), i1, tD(i));
    npFullV = v1;
    npFullD = i1 + 1;
    // chain indexed from the top: k = 0 is the largest tap, whose
    // neighbour is the attack window; tap i sits at k = nB-1-i.
    // (v0.9's npKV(0) select2 has equal branches here, so it is gone.)
    npKV(0) = v1;
    npKD(0) = select2(v1 < sV(nB-1), npFullD, i1);
    npKV(k) = select2(sV(nB-k) < sV(nB-1-k), npKV(k-1), sV(nB-k));
    npKD(k) = select2(sV(nB-k) < sV(nB-1-k), npKD(k-1), sD(nB-k));
    npTV(i) = npKV(nB-1-i);
    npTD(i) = npKD(nB-1-i);

    // shared helpers (op bound to minIdxOp; the pair fold is a balanced
    // tree -- min with leftmost tie-break is associative, so the result
    // is bit-identical to the sequential fold, just a shorter
    // dependency chain):
    sequentialOperatorParOut(N) = seq(i, N, operator(i));
    operator(i) = si.bus(2*i),
        (si.bus(2) <: (si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))) : minIdxOp)));
    combineTree(1) = si.bus(2);
    combineTree(2) = minIdxOp;
    combineTree(N) = (combineTree(half), combineTree(N - half)) : minIdxOp
    with { half = int(N/2); };
    isUsed(m, mMax, i) = ba.take(i+1, (int2bin(m,(mMax-1)*2+1)));
    sumPrevBlocks(m, mMax, 0) = 0;
    sumPrevBlocks(m, mMax, i) = (ba.subseq((allBlockSizes(m, mMax)),0,i):>_);
    allBlockSizes(m, mMax) = par(j, maxNrBits(mMax-1), (pow2(j)) * isUsed(m, mMax, j));
    maxNrBits(m) = int2nrOfBits(m);
    pow2(i) = 1<<i;
    int2bin(v,m) = par(j, maxNrBits(m-1), int(floor((v)/(pow2(j))))%2);
    int2nrOfBits(v) = int(floor(log(v)/log(2))+1);
};

//========================================================================
// smoother
//========================================================================

//------------------------`hermiteAttackFollower`------------------------
// Event-driven gain envelope, attack side only: chases the critical
// constraint with latched Hermite segments; between legs it TRACKS the
// playing constraint (instant release).
//
// #### Usage
//
// ```
// hermiteAttackFollower(nC, cands, ceilPlay) : _
// ```
//
// Where:
//
// * `nC`: number of candidates (compile-time int)
// * `cands`: 4*nC signals as in v0.9: (value, deadline, next-deeper
//   value, next-deeper deadline) per candidate. The deadline is the
//   exact play index; the next-deeper pair becomes the landing chord
//   (own value as next-deeper value = land flat). Disabled candidates
//   read (ma.MAX, 1, _, _) and never win.
// * `ceilPlay`: the constraint sample now playing (the raw GR delayed
//   by the smoother latency). Whenever the follower is idle every
//   candidate value is >= gain, and ceilPlay >= v1 >= gain, so
//   tracking it never jumps down.
//
// Per sample, every candidate gets (requiredSlope, value, deadline,
// npVal, npDl) with requiredSlope = (value - gain)/deadline; the
// critical candidate is the argmin (steepest descent required), and
// its chord (npVal - value)/(npDl - deadline) is the landing slope.
//
// Trigger:
// * attack: critVal < gain, and (critVal != p1, or critDl < T-k, or
//   arrived). The `arrived` term is new vs v0.9: idle tracking moves
//   gain off the latched p1, so critVal == p1 no longer implies the
//   leg is already handled. On a running leg it is 0, so in-leg
//   behavior is verbatim v0.9.
// * no release triggers. Idle output = ceilPlay; on the sample after
//   a landing the target is playing, so ceilPlay = p1 and the target
//   is still held through its play sample, as in v0.9.
//----------------------------------------------------------------------
hermiteAttackFollower(nC, cands, ceilPlay) =
    (loop ~ si.bus(7)) : (_, si.block(6))
with {
    // state: gain, p0, m0, p1, m1, k, T (previous-sample values inside loop)
    loop(gain, p0, m0, p1, m1, k, T) =
        gainN, p0N, m0N, p1N, m1N, kN, TN
    with {
        dirPrev = gain - gain';        // current slope, units/sample

        // ---- critical-constraint selection ----
        trip(val, dl, npv, npd) =
            (val - gain) / max(1, dl), val, dl, npv, npd;
        scored  = cands : par(i, nC, trip);
        crit    = scored : red5(nC);
        critVal = crit : (!, _, !, !, !);
        critDl  = crit : (!, !, _, !, !);
        critNpV = crit : (!, !, !, _, !);
        critNpD = crit : (!, !, !, !, _);
        amin5(sa,va,da,ua,wa, sb,vb,db,ub,wb) =
            select2(pk, sa, sb), select2(pk, va, vb), select2(pk, da, db),
            select2(pk, ua, ub), select2(pk, wa, wb)
        with { pk = sb < sa; };
        // balanced tree: leftmost-min selection is associative, so the
        // winner matches the sequential fold, with a dependency chain
        // of ceil(log2(nC)) instead of nC-1 selects -- and this chain
        // sits on the feedback loop's critical path (the divisions all
        // run in parallel ahead of it).
        red5(1) = si.bus(5);
        red5(2) = amin5;
        red5(N) = (red5(half), red5(N - half)) : amin5
        with { half = int(N/2); };

        // ---- trigger ----
        arrived = k > T;               // previous leg done, tracking
        attNeed = critVal < gain;
        // re-latch when the critical value changes, when its (exact)
        // deadline undercuts the running leg's remaining time
        // (equal-depth peak that plays sooner: plateaus), or when idle
        // (tracking has moved gain off p1, so the != test alone no
        // longer suffices). On a steady leg all three stay quiet.
        attTrig = attNeed & ((critVal != p1) | (critDl < (T - k)) | arrived);
        trig    = attTrig;

        // ---- new-segment values (only used when trig == 1) ----
        Tt    = max(1, critDl);
        p1t   = critVal;
        delta = (p1t - gain) / Tt;     // average slope, sign = direction
        // every landing aims at the nearest strictly-deeper point one
        // scale out (the critical candidate's np pair); the chord is 0
        // when nothing deeper is in sight (sentinel: npVal = own value)
        aimDn = (critNpV - critVal) / max(1, critNpD - critDl);
        // Fritsch-Carlson monotone bound; delta < 0 whenever trig fires
        lo    = 3 * delta;
        // v0.3: the launch tangent is velocity-continuous in BOTH
        // directions, with the FC bound kept on the downside only.
        // dirPrev is the constraint's own tracked slope on launches
        // from idle (gain == ceilPlay there) and the running hump's
        // decaying slope on re-latches, so every re-plan keeps C1.
        // No mirrored cap and no arrived split -- see the header for
        // why both defeated the pickup, and why re-plans cannot
        // chain humps without the clamp.
        m0t   = max(lo, dirPrev);
        m1t   = max(lo, min(0, aimDn));

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
        // idle TRACKS the playing constraint (instant release)
        gainN   = select2(gliding, ceilPlay, hermiteVal);
    };
};

//-----------------------`lookaheadAttackSmoother`------------------------
// Attack-only smoother wiring: the bank, the follower, and the playing
// constraint. Input is the RAW GR signal.
//
// #### Usage
//
// ```
// _ : lookaheadAttackSmoother(nAtt, maxAtt) : _
// ```
//
// Latency: nAtt - 1 samples; delay the raw GR (and the audio in a full
// limiter) by the same amount to line up with the output. Release is
// the caller's job: this stage tracks rises instantly.
//----------------------------------------------------------------------
lookaheadAttackSmoother(nAtt, maxAtt, rawGR) =
    hermiteAttackFollower(nB + 1, cands, ceilPlay)
with {
    nB       = int(floor(log(maxAtt)/log(2)) + 1);
    // the bank output is the follower's candidate list: (value,
    // deadline, npV, npD) for the attack window, then for every tap
    cands    = rawGR : slidingMinIdxBankAtt(nAtt, maxAtt);
    ceilPlay = de.delay(maxAtt - 1, nAtt - 1, rawGR);
};

//-------------------------------- demo ---------------------------------
// out1: delayed raw GR (the constraint the smoother must stay <= )
// out2: smoother output
//
// Brickwall check: out2 <= out1 at every sample (no hard clamp, so any
// violation measures the residual shadowed-peak gap). When idle,
// out2 == out1 exactly (tracking).

MainGroup(x) = hgroup("[0]hermiteAttackSmoother", x);
TestGroup(x) = vgroup("[0]Test signal", x);
SmootherGroup(x) = vgroup("[1]Smoother", x);

// --- Test signal ---
testNoiseLevel = TestGroup(hslider("[0]noise level", 0, 0, 1, 0.001));
testNoiseRate = TestGroup(hslider("[1]noise rate", 42, 1, 1000, 1));
testBlockscale = TestGroup(hslider("[2]blockscale", 1, 0.01, 10, 0.01));
testFreq = TestGroup(hslider("[3]freq", 1, 0.001, 30, 0.001));
testStep1 = TestGroup(hslider("[4]step1", 0.75, -1, 1, 0.001));
testStep2 = TestGroup(hslider("[5]step2", 0.125, -1, 1, 0.001));
testSelect = TestGroup(hslider("[6]signal select", 0, 0, 2, 1));
testSignal = select3(testSelect, testSignal1, testSignal2, testSignal3);
testSignal1 = it.interpolate_linear(testNoiseLevel,
    (loop~_),
    no.lfnoise(testNoiseRate))
with {
    loop(prev) = no.lfnoise0(testBlockscale*(abs(prev*69)%9:pow(0.75)*5+1));
};
testSignal2 = os.lf_squarewave(testFreq)*0.5;
// the torture signal through an instant-attack / one-pole-release
// follower: raw GR as it looks when release is done upstream --
// descents stay steps (the lookahead's job), every rise is a smooth
// exponential, so attacks launch from a MOVING constraint. This is
// the signal the v0.2 direction pickup is for.
testRelMs = TestGroup(hslider("[7]upstream release [unit:ms]", 50, 1, 500, 1));
testSignal3 = testSignal1 : relFollow
with {
    relCoef = exp(-1.0 / (testRelMs * 0.001 * ma.SR));
    relFollow(x) = loop ~ _
    with { loop(y) = min(x, x + (y - x) * relCoef); };
};

// --- Smoother parameters ---
// compile-time maximum: 50 ms at maxSR. Lower maxSR if you never run
// above 48/96k, to save memory and a few reduce stages.
maxSR  = 192000;
maxAtt = int(0.05 * maxSR);

attMs = SmootherGroup(hslider("[0]attack lookahead [unit:ms]", 25, 0, 50, 0.1));
nAtt  = max(2, min(maxAtt, int(attMs * 0.001 * ma.SR)));

process = MainGroup(demo(testSignal))
with {
    demo(rawGR) = grPlay, smoothed
    with {
        grPlay   = de.delay(maxAtt - 1, nAtt - 1, rawGR);
        smoothed = lookaheadAttackSmoother(nAtt, maxAtt, rawGR);
    };
};
