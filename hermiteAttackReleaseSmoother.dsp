declare name "hermiteAttackReleaseSmoother";
declare version "0.2";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

//========================================================================
// Attack + release lookahead smoother: hermiteAttackSmoother v0.4 with
// rises handled by Hermite legs instead of instant tracking. The attack
// side is untouched: same bank, same candidate scoring, same trigger
// algebra, same creep gate. The v0.9 release machinery (direction
// lookahead nExtra, big window, release ceiling, freeze) stays deleted
// -- this release needs none of it.
//
// The release in one sentence: when the follower is at rest and the
// window min v1 has risen above the held gain, latch a Hermite leg
// from (gain, dirPrev) up to (v1, flat) over nRel samples; otherwise
// hold.
//
// Why the target is v1 (the attack-window min), not the playing
// sample:
// * gain <= v1 is brickwall by construction: v1 is the min over the
//   next nAtt samples, and a release leg cannot rise above its own
//   target (FC cap, below), so it cannot rise above anything about
//   to play.
// * the release gets lookahead for free: between two peaks closer
//   than the window, v1 never rises, so the gain never starts a rise
//   it would have to attack back out of -- no pumping wiggle, and no
//   nExtra/big-window machinery to pay for it. v1 already exists as
//   the bank's first output.
// * hold-until-the-peak falls out: the landed target stays the window
//   min until its sample PLAYS and leaves the window (every scale
//   shares that trailing edge), so gain holds at the peak's depth
//   through the peak's play sample -- v0.4's hold-the-target
//   convention -- and only then releases.
//
// Semantics changes vs v0.4 (attack-only):
// * Idle no longer TRACKS ceilPlay; it HOLDS gain. Rises are legs.
//   ceilPlay is gone from the follower and the wiring (the demo still
//   delays the raw GR for the brickwall check), which also drops the
//   follower's maxAtt-long ceilPlay delay line.
// * relTrig = v1 > p1: latch from rest AND re-latch mid-release-leg
//   whenever the window min rises above the running target (at rest
//   gain == p1, so this reads v1 > gain there). Mutually exclusive
//   with attNeed by construction: every candidate is >= v1, so
//   v1 > p1 (>= gain) implies critVal > gain -- and mid-attack-leg
//   it can never fire, since the latched target's sample stays
//   inside the window until the leg lands, so v1 <= p1 there.
// * (v0.2) v0.1 latched only from rest (arrived & v1 > gain) and
//   flew every release leg to touchdown before looking at v1 again.
//   On noisy material a big rise is revealed as a stream of
//   window-min micro-steps (each noise dip exits the window one at
//   a time), so one release became a chain of full-nRel micro-legs
//   with velocity pinned to 0 at every joint -- while the leg flew
//   to its stale, barely-higher target, the gain HELD; then it
//   crawled one micro-step; then held again. Exactly the "holds or
//   releases slowly where it could release fast" symptom, and block
//   input never showed it because a block reveals its rise in ONE
//   window-min step. Re-latching on every rise of v1 rides the
//   reveal instead: a creeping v1 becomes a per-sample re-plan
//   whose velocity is governed by the FC cap at 3*(v1 - gain)/nRel
//   -- fast when far, gentle when near, no zero-velocity joints --
//   while a stepped v1 still flies whole legs (v1 == p1 in flight),
//   so block behavior is unchanged: one S-curve, exactly nRel. A v1
//   that DROPS back under a flying target is flown past unchanged
//   (re-targeting down would clamp a fast rise onto the FC cap in
//   one sample: a corner); if the gain crosses it, attNeed catches
//   the crossing with a smooth arc, as before.
// * A release leg cannot overshoot its target: with m1 = 0,
//   p - p1 = (1-tau)^2 * ((1+2*tau)*(p0-p1) + tau*T*m0)
//          <= (1-tau)^2 * (p1-p0) * (tau-1) <= 0
//   for any m0 <= 3*delta -- including a (safe, downward) dip when
//   dirPrev < 0 at launch. Hence m0 = min(3*delta, dirPrev): the same
//   Fritsch-Carlson bound as the attack, mirrored -- a cap instead of
//   a floor.
// * Attacks that fire mid-rise pick up OUR release leg's velocity as
//   dirPrev -- the attack-side v0.2/v0.3 pickup unchanged in form, now fed by the
//   internal release instead of an upstream one-pole. testSignal3
//   (upstream release) is kept only as an A/B reference.
//
// Cost, against the v0.4 numbers (attack-only, isolated algorithm,
// 115 ns/sample, sizeof(dsp) 2.4 MB): no new divisions (delta was
// already on the trigger path and is shared; aimDn is untouched), no
// new state (same 7-wide loop), no new delay lines, cascade and bank
// verbatim. The audio hot path gains one compare (v1 > gain), one
// AND, one OR, four select2 and a min/max pair -- noise next to the
// per-candidate divisions and the argmin tree that dominate. nRel is
// slider-derived, so its guard runs in the control block. The wiring
// LOSES the ceilPlay tap (a maxAtt-long delay line). Latency
// unchanged: nAtt - 1. (Reasoned, not yet measured -- bench it.)
//
// Release semantics: fixed DURATION, not fixed time constant -- any
// rise, large or small, takes nRel samples. An isolated peak fully
// recovers in exactly nRel; a staircase of k window-min steps takes
// up to k*nRel. nRel = 1 gives instant rises to the window min (note:
// v0.4 idle tracked ceilPlay, i.e. the playing sample -- same feel,
// safer target).
//
// Known limitations, on purpose:
// * The shadowed-second-deepest gap of v0.9/v0.4 is unchanged.
// * The v0.3 hump class -- an attack launched off a rising tangent
//   can poke briefly above a flat-playing constraint before the
//   deadline-0 candidate clamps it -- now also applies to rises made
//   by internal release legs, since those are where rising tangents
//   come from. Same escape hatch if out <= grPlay must be bit-exact:
//   min the output with the delayed raw GR, at the price of a C1
//   corner at the touch point.
// * A v1 that drops back to just above the gain mid-rise is flown
//   past rather than re-targeted (see relTrig): the transient can
//   arc above the momentary window min -- inside the v0.3 hump
//   class, per-play-time deadlines still enforced -- before attNeed
//   reels it in. Re-targeting down would trade that arc for a
//   velocity corner.
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

//---------------------`hermiteAttackReleaseFollower`--------------------
// Event-driven gain envelope: chases the critical constraint with
// latched Hermite attack legs (verbatim v0.4); between attack legs it
// rises toward the window min v1 with Hermite release legs of nRel
// samples, and HOLDS while v1 == gain.
//
// #### Usage
//
// ```
// hermiteAttackReleaseFollower(nC, nRel, cands) : _
// ```
//
// Where:
//
// * `nC`: number of candidates (compile-time int)
// * `nRel`: release leg length in samples (>= 1, may vary at control
//   rate); 1 = instant rises to v1
// * `cands`: 4*nC signals as in v0.9: (value, deadline, next-deeper
//   value, next-deeper deadline) per candidate. The deadline is the
//   exact play index; the next-deeper pair becomes the landing chord
//   (own value as next-deeper value = land flat). Candidate 0 MUST be
//   the full attack window: its value doubles as the release target
//   v1. Disabled candidates read (ma.MAX, 1, _, _) and never win.
//
// Per sample, every candidate gets (requiredSlope, value, deadline,
// npVal, npDl) with requiredSlope = (value - gain)/deadline; the
// critical candidate is the argmin (steepest descent required), and
// its chord (npVal - value)/(npDl - deadline) is the landing slope.
//
// Triggers:
// * attack: verbatim v0.4, creep gate included.
// * release: v1 > p1 -- latch from rest (gain == p1 there) and
//   re-latch mid-release-leg whenever the window min rises above the
//   running target; a target that drops back is flown past. Mutually
//   exclusive with attNeed since every candidate >= v1, and inert
//   mid-attack-leg since the latched target stays in the window
//   until touchdown (v1 <= p1 there).
// * idle (arrived, v1 == gain) holds gain. The landed target stays
//   the window min through its own play sample, so peaks are held
//   through the peak exactly as in v0.4.
//----------------------------------------------------------------------
hermiteAttackReleaseFollower(nC, nRel, cands) =
    (loop ~ si.bus(7)) : (_, si.block(6))
with {
    // the release target: candidate 0's value = the attack-window min
    v1 = cands : ba.selector(0, 4*nC);

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

        // ---- triggers ----
        arrived = k > T;               // previous leg done, at rest
        attNeed = critVal < gain;
        // re-latch when the critical value changes, when its (exact)
        // deadline undercuts the running leg's remaining time
        // (equal-depth peak that plays sooner: plateaus), or when at
        // rest (a release leg may have moved gain off p1, so the !=
        // test alone no longer suffices). On a steady leg all three
        // stay quiet.
        // (v0.4) EXCEPT: a changed minimum that plays at-or-after the
        // running leg's arrival and demands no steeper descent than
        // the leg has left is flown past, not re-latched -- the creep
        // gate; see the v0.4 header. Chords are compared
        // cross-multiplied (both deadlines >= 0, so the direction is
        // preserved and no division lands on the loop's critical
        // path; rRem = 0 makes steeper false, so a landed leg still
        // holds its target through the play sample and re-launches
        // via `arrived`). During a release leg p1 - gain > 0 while
        // critVal - gain < 0, so steeper is true and the creep gate
        // can never swallow an attack that fires mid-rise.
        rRem    = max(0, T - k);       // remaining steps of the leg
        steeper = ((critVal - gain) * rRem) < ((p1 - gain) * critDl);
        flyOn   = (steeper == 0) & (critDl >= rRem);
        attTrig = attNeed & (((critVal != p1) & (flyOn == 0))
                             | (critDl < rRem) | arrived);
        // release: latch or re-latch whenever the window min rises
        // above the running target. At rest gain == p1, so this
        // reads v1 > gain: launch (no trigger at v1 == gain: hold,
        // which still covers a landed attack target through its own
        // play sample). Mid-release-leg it re-plans toward the new
        // v1 from (gain, dirPrev) with the horizon reset to nRel --
        // (v0.2) v0.1 latched only from rest, which turned noisy
        // rises (revealed as window-min micro-steps) into chains of
        // full-nRel micro-legs that held or crawled; see the header.
        // Mid-attack-leg it can never fire (the latched target's
        // sample stays in the window until the leg lands, so
        // v1 <= p1 there), and v1 > p1 >= gain forces attNeed = 0,
        // so the two triggers stay mutually exclusive and the
        // segment selects below can key on relTrig alone. A v1 that
        // drops back under a flying release target is flown past;
        // if the gain crosses it, attNeed catches the crossing with
        // a smooth arc.
        relTrig = v1 > p1;
        trig    = attTrig | relTrig;

        // ---- new-segment values (only used when trig == 1) ----
        Tt    = max(1, select2(relTrig, critDl, nRel));
        p1t   = select2(relTrig, critVal, v1);
        delta = (p1t - gain) / Tt;     // average slope, sign = direction
        // every attack landing aims at the nearest strictly-deeper
        // point one scale out (the critical candidate's np pair); the
        // chord is 0 when nothing deeper is in sight (sentinel:
        // npVal = own value). Release legs land flat instead.
        aimDn = (critNpV - critVal) / max(1, critNpD - critDl);
        // Fritsch-Carlson bound: a launch floor on attacks
        // (delta < 0), a launch cap on releases (delta > 0)
        lo    = 3 * delta;
        // v0.3: the launch tangent is velocity-continuous in BOTH
        // directions, with the FC bound kept on the overshoot side
        // only -- the downside for attacks, the upside for releases.
        // dirPrev is a release leg's decaying slope when an attack
        // fires mid-rise (the attack-side v0.2/v0.3 pickup) and ~0
        // on launches from a hold. m0 <= 3*delta with m1 = 0 keeps a
        // release leg <= its target everywhere (see the header), so
        // gain <= v1 survives the whole leg; under per-sample
        // re-latch (a creeping v1) the same cap doubles as the
        // approach governor: velocity <= 3*(v1 - gain)/nRel, fast
        // when far, gentle when near. The safe direction (a dip when
        // dirPrev < 0) is left uncapped, as on the attack side.
        m0t   = select2(relTrig, max(lo, dirPrev), min(lo, dirPrev));
        m1t   = select2(relTrig, max(lo, min(0, aimDn)), 0);

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
        // at rest the output HOLDS: v1 == gain there (v1 > gain fires
        // a release leg, v1 < gain an attack), and rises are legs now
        gainN   = select2(gliding, gain, hermiteVal);
    };
};

//--------------------`lookaheadAttackReleaseSmoother`-------------------
// Full smoother wiring: the bank and the follower. Input is the RAW GR
// signal -- descents AND rises are shaped here, so no upstream release
// stage is needed.
//
// #### Usage
//
// ```
// _ : lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt) : _
// ```
//
// Latency: nAtt - 1 samples; delay the raw GR (and the audio in a full
// limiter) by the same amount to line up with the output.
//----------------------------------------------------------------------
lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR) =
    hermiteAttackReleaseFollower(nB + 1, nRel, cands)
with {
    nB    = int(floor(log(maxAtt)/log(2)) + 1);
    // the bank output is the follower's candidate list: (value,
    // deadline, npV, npD) for the attack window, then for every tap
    cands = rawGR : slidingMinIdxBankAtt(nAtt, maxAtt);
};

//-------------------------------- demo ---------------------------------
// out1: delayed raw GR (the constraint the smoother must stay <= )
// out2: smoother output
//
// Brickwall check: out2 <= out1 at every sample, up to the v0.3 hump
// class (no hard clamp, so any violation measures that residual plus
// the shadowed-peak gap). Rises are S-curves toward the window min:
// gain holds through each peak's play sample, then recovers in nRel.
// release = 0 gives instant rises to the window min (v0.4 tracked the
// playing sample instead -- same feel, safer target).

MainGroup(x) = hgroup("[0]hermiteAttackReleaseSmoother", x);
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
// exponential, so attacks launch from a MOVING constraint. Redundant
// now that release lives inside the smoother; kept as an A/B
// reference against the upstream-release architecture.
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
relMs = SmootherGroup(hslider("[1]release [unit:ms]", 50, 0, 500, 0.1));
nRel  = max(1, int(relMs * 0.001 * ma.SR));

process = MainGroup(demo(testSignal))
with {
    demo(rawGR) = grPlay, smoothed
    with {
        grPlay   = de.delay(maxAtt - 1, nAtt - 1, rawGR);
        smoothed = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR);
    };
};
