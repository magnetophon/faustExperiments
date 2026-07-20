declare name "hermiteAttackReleaseSmoother";
declare version "1.0";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

//========================================================================
// Attack + release lookahead smoother. ONE Hermite-leg follower shapes
// both directions of a gain-reduction signal: descents chase the
// critical constraint read from a dyadic candidate bank (lookahead
// attacks), rises are latched Hermite release legs toward the
// attack-window min. No upstream release stage is needed, and idle
// HOLDS the gain rather than tracking the playing sample.
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
//   extra lookahead machinery to pay for it. v1 already exists as
//   the bank's first output.
// * hold-until-the-peak falls out: the landed target stays the window
//   min until its sample PLAYS and leaves the window (every scale
//   shares that trailing edge), so gain holds at the peak's depth
//   through the peak's play sample -- and only then releases.
//
// relTrig = v1 > p1: latch from rest AND re-latch mid-release-leg
// whenever the window min rises above the running target (at rest
// gain == p1, so this reads v1 > gain there). Mutually exclusive
// with attNeed by construction: every candidate is >= v1, so
// v1 > p1 (>= gain) implies critVal > gain -- and mid-attack-leg
// it can never fire, since the latched target's sample stays
// inside the window until the leg lands, so v1 <= p1 there.
// Re-latching on every rise of v1 matters on noisy material, where
// a big rise is revealed as a stream of window-min micro-steps
// (each noise dip exits the window one at a time). Latching only
// from rest would turn one release into a chain of full-nRel
// micro-legs with velocity pinned to 0 at every joint: while a leg
// flew to its stale, barely-higher target, the gain would HOLD;
// then crawl one micro-step; then hold again -- holding or
// releasing slowly where it could release fast, and block input
// would never show it, because a block reveals its rise in ONE
// window-min step. Riding the reveal instead, a creeping v1
// becomes a per-sample re-plan whose velocity is governed by the
// FC cap at 3*(v1 - gain)/nRel -- fast when far, gentle when near,
// no zero-velocity joints -- while a stepped v1 still flies whole
// legs (v1 == p1 in flight), so block input gets one S-curve of
// exactly nRel. A v1 that DROPS back under a flying target is
// flown past unchanged (re-targeting down would clamp a fast rise
// onto the FC cap in one sample: a corner); if the gain crosses
// it, attNeed catches the crossing with a smooth arc, as on any
// attack.
//
// Release taps, mirror-aligned: a second read of the SAME cascade,
// anchored at the opposite edge. The attack taps slide every stage
// into place to share the playing edge (delay nAtt - 2^i); the
// release taps are the raw stages themselves -- suffix j = the
// last 2^j samples of the window, all sharing the window's newest
// sample -- zero delay lines, zero new stages. Nested suffixes
// sample the CEILING SCHEDULE C(s) = min over play [s, nAtt) at
// dyadic distances from the far edge: the shape of the raw
// release. The bank condenses them into ONE next-higher pair
// (nhV, nhD): the level the ceiling lifts to when the pinned min
// plays out (the largest suffix excluding i1; dyadic resolution
// can read HIGH -- the mirror of the shadowed-second-deepest gap;
// ties where the min value recurs later give nhV == v1, the flat
// sentinel), plus the play index of that level's own min. The
// pair drives the lift-aware ride (below); it is also selected
// into the shared landing-chord endpoints for release legs, but
// the release LANDS FLAT and flies T = nRel from rest -- the two
// obvious schedule-aware landing shapes (aim the landing slope
// into the coming rise; stretch T to land ON the pin's play
// sample so the lift re-latch carries velocity) both measured as
// regressions on the noisy workload, so they ship NEUTRALIZED
// with the chord endpoints still selected, keeping the pair live
// in the hot path for experiments. Why more landing information
// cannot fix the remaining crawls: at an attack landing the gain
// is pinned AT v1 through its play sample (it must be), so C1
// forces velocity ~0 into every such lift -- the rebuild there is
// the constraint, not a knowledge gap.
//
// The lift-aware ride. The v1-chase brakes as its gap closes (the
// drive term 3*(v1 - gain)/T^2 vanishes) -- wasted braking
// whenever the pin plays out before the gain could reach it
// anyway. While a strictly higher level is scheduled
// (liftAhead = nhV > v1), the launch may keep the boosted chase
// velocity -- dirPrev plus the farther level's drive,
// (nhV - v1) * 3/nRel^2 per sample, a control-rate constant --
// capped by the fastest rate that cannot cross v1 before the
// lift: rideMax = (v1 - gain)/(i1 + 1). Safety is per-sample
// INDUCTION, not a curve property: one step at v <= rideMax
// leaves gain <= v1, and the trigger re-plans every sample while
// liftAhead (term 2) or while a relaxed launch is latched
// (term 3, the flown-whole guard: m0*T > 3*(p1 - p0), computable
// from latched state alone, release legs only, with a 1e-8
// relative margin so FC-capped launches never trip it). Both
// terms yield to attNeed -- unlike v1 > p1 they do not imply
// v1 > gain, and a missed attack is a brickwall leak. Isolated
// single-step rises never see liftAhead (the post-lift window is
// single-level, nhV == v1), so the calibrated exact-nRel block
// S-curve is preserved; on multi-level material the ride roughly
// halves the stall fraction a plain v1-chase leaves on the noisy
// workload and cuts its brickwall residual, with blocks
// unchanged. Accepted corners, both bounded: a pin that RECURS at
// the wall (nhV read high in the dyadic blind spot, or the i1 = 0
// rounding, leak <= 3*gap/T^2, orders below the hump class) parks
// the ride at v1 -- as a flat landing, since the momentum
// re-latch (below) starts the deceleration when the cap first
// binds instead of at the wall; attacks that fire mid-ride pick
// up the hotter dirPrev, growing their hump within the documented
// class.
//
// Momentum-preserving release re-latch. A re-latch that CLAMPS
// the launch velocity onto its cap -- the fresh-leg FC cap
// 3*(v1 - gain)/nRel, which any window-min creep past a flying
// leg's midpoint lands on (for a whole leg re-latched at phase
// tau, cap/velocity = (1-tau)(1+2tau)/(2tau) < 1 for tau > 1/2),
// or max(FC cap, rideMax) the sample a lift blip appears (nhV one
// noise-hair above v1 flips liftAhead while a leg is hot) -- is a
// one-sample velocity corner: a kink on the release on noisy
// material. Instead, when dirPrev exceeds the fresh-leg cap
// (dirPrev*nRel > 3*(v1 - gain), cross-multiplied, no division on
// the compare), SHORTEN the leg: T = ceil(3*(v1 - gain)/dirPrev)
// puts the FC bound exactly at dirPrev, so the launch keeps its
// velocity and decelerates smoothly into the target, landing
// early rather than braking instantly. The no-overshoot lemma
// (below) holds at m0 = 3*delta with equality, so gain <= v1
// survives unchanged. At the governor boundary Tshort == nRel:
// the select2 introduces no step of its own, the steady creep
// governor is untouched, and launches from rest (dirPrev ~ 0)
// plus whole-leg block S-curves are unaffected (capped false
// there). ceil keeps T integer -- a fractional T never reaches
// tau = 1, parking a landed leg epsilon shy of its target and
// deadlocking the v1 > p1 trigger -- and puts m0*T exactly ON
// 3*(p1 - p0), inside the flown-whole guard's 1e-8 margin, so
// term 3 stays quiet on these launches. Cost: one audio-rate
// division + ceil on the trigger path.
//
// Release launch floor. The launch tangent is velocity-continuous
// upward only: m0 = min(3*delta, max(0, dirPrev)). A negative
// dirPrev would launch a "safe, downward dip" (brickwall-safe --
// h10 >= 0 keeps the flight <= v1 regardless), but a release
// targets v1, BY DEFINITION the deepest point in the total
// lookahead: nothing below it exists to descend toward, so any
// dip is gratuitous over-reduction. And the dip is reachable: an
// attack re-latch chain lands ON its pin's play sample (deadline
// 0, no hold sample in between), the pin leaves the window on the
// NEXT sample, and term 1 fires with dirPrev = the final approach
// step -- or worse, a hump's deadline-0 clamp step. With T = nRel
// the Hermite integrates that tangent into a swoop of up to
// ~0.15*T*|dirPrev| below the bottom (measured on the noise
// workload: excursions to 0.85 below the window min, hundreds of
// samples long). Floored at 0, the dip is replaced by a flat
// launch whose one-sample velocity corner is the approach step
// that was already there; momentum re-latches (dirPrev > 0) and
// launches from a hold (dirPrev == 0) are untouched, so block
// S-curves and the shortened-leg machinery are unaffected.
// Invariant: the gain never DESCENDS below the current window min
// -- attack flights stay >= their target >= v1 (FC box, or
// all-nonnegative Hermite terms when m0 > 0), release flights
// stay >= their launch point.
//
// Flat landing at the window-deepest point. Flat landing lives
// inside the ordinary candidate law, which already paces every
// (value, deadline) at every scale: the landing chord is flat
// EXACTLY when the critical candidate is the window-deepest point
// (the next-deeper sentinel copies the candidate's own value --
// an exact compare, no tolerance; candidate 0's chord is the flat
// sentinel unconditionally), and m1 = 0 falls out of the aim path
// untouched. The one thing the ordinary law needs on top is entry
// manners: a flat-chord leg latched hotter than its full-deadline
// FC floor (dirPrev * critDl < 3 * (critVal - gain),
// cross-multiplied, both sides negative there) would CLAMP onto
// m0 = 3*delta -- a kink planted right at the entry. Same
// medicine as the release re-latch, mirrored: SHORTEN,
// attT = ceil(3*(critVal - gain)/dirPrev), putting the floor
// exactly ON dirPrev -- the entry keeps its velocity, decelerates
// smoothly, lands at v1 EARLY and holds flat through the pin's
// play sample. Early landing at the window-deepest is safe by
// construction (every candidate value is >= v1, so nothing binds
// until the pin plays out), and the mirrored no-undershoot lemma
// holds at m0 = 3*delta with equality, so gain >= v1 survives.
// Cool entries and sloped chords fly T = critDl untouched (the
// select2 branches agree at the boundary), so block S-curves are
// unaffected; the trigger algebra stays live throughout, so a dip
// playing under any flight re-latches like any attack -- nothing
// is blinded. Bottoms the creep clamp chain still owns (the pin
// never becomes critical before the endgame) land at the launch
// floor's terminal step: a residual corner class orders below the
// two removed here. Cost: one division on the latch path (attTs),
// the exact mirror of Tshort.
//
// The ceiling zipper, and the two terms that kill it. On smooth
// descents (raw ramping down, a deep pin far out) the law
// correctly makes the far pin critical and latches the correct
// flat-landing leg -- but a latch that only happens at `arrived`,
// one sample AFTER the play-hold, reads dirPrev = 0 (the previous
// clamp's velocity has already died), so the long S-curve
// launches near-flat, the ceiling outruns it within a few
// samples, the deadline-1 raw forces a T = 1 clamp (C0), one hold
// sample zeroes dirPrev, repeat: a sawtooth riding just ABOVE the
// raw (the brickwall leak class) with a curvature corner per
// cycle -- "bumping into the ceiling". Two terms fix it:
// * the LANDING HANDOFF: when deeper work is pending, attTrig
//   also fires on the first post-landing sample (k == T), where
//   dirPrev IS the landing step -- the chord aim (or a clamp)
//   finally hands its velocity to the next leg instead of dying
//   in the hold. attNeed gates it, so peak holds and release
//   holds are untouched, and descending through a shallower
//   pin's play sample is safe (gain <= its value throughout).
// * the CLEARANCE-CHECKED LAUNCH: at every latch the planned
//   cubic is evaluated at every candidate deadline (T^3-scaled,
//   division-free, float-coerced -- d^3 overflows int32;
//   min(val, 2) caps the disabled-tap sentinel, safe since any
//   GR is <= 1); if any p(dl) > val, the plan is refused and
//   SHORTENED (below). Isolated block steps clear the check and
//   render untouched.
// Model A/B over the worst zipper episode: curvature corners
// > 1e-3 gone entirely, the brickwall leak to exactly 0, at a
// small mean extra reduction inside the episode -- the leg dives
// below the hugging path, the safe direction. Cost: one compare
// (handoff) plus ~10 multiplies and two compares per candidate
// per sample (check), no divisions, no state, no delay lines.
//
// C1 clearance: on a failed check the leg is SHORTENED, velocity
// kept -- the shorten medicine, third verse. (Replacing the
// launch velocity with the critical's required slope instead
// would be a one-sample velocity corner at the exact sample the
// plan first diverges from the reference path: rising entries --
// a deep pin entering the window under a hot release leg --
// chopped onto a near-flat slope, rest entries snapped from 0
// onto it.) Shortening is always the safe direction: at fixed
// play time t, dp/dT = 2*tau*(1-tau)*t*(m0 - 3*delta)/T >= 0
// (m0 >= 3*delta = lo by construction, in every regime including
// m0 riding lo), so a shorter leg only ever lowers the path --
// failed candidates are the only ones that need re-checking,
// passing ones stay clear. Two regimes, split on the entry
// direction:
// * dirPrev > 0 (the chop class): land EARLY. Tclr = the
//   smallest FAILED deadline. Every failed candidate moves into
//   the dl >= T auto-pass (the leg sits landed at p1 by then,
//   and every candidate due at-or-before critDl has val >=
//   critVal -- deeper-and-sooner would have won the argmin),
//   every passing one stays clear by monotonicity: exact, and
//   division-free (a min tree over select2(pass, dl, 1e30)).
//   The arc a rising entry needs in order to turn around C1
//   still flies -- compressed into the shortened leg, its peak
//   scaling with T -- instead of being chopped flat.
// * dirPrev <= 0 (the zipper class): let CURVATURE do it. The
//   m0 = 0 cubic bounds every m0 <= 0 plan from above, and
//   q(u) = 3u^2 - 2u^3 >= u^2 gives the closed-form sufficient
//   condition tau >= sqrt(s), s = (gain - val)/(gain - p1):
//   Tclr^2 = (gain - p1) * min over failed of dl^2/(gain - val),
//   the min run cross-multiplied as a (num, den) pair tree --
//   ONE division + ONE sqrt total, ceil'd like Tshort.
//   Overshortens by at most sqrt(3) in the small-gap limit; the
//   safe direction again.
// The engagement sample is gated by the exact check (written
// with m0*T and m1*T as division-free products, so it does not
// wait on delta): the plan refuses, at the earliest possible
// sample, to fly a schedule that would outrun it -- and leaves
// the reference path tangentially rather than with a corner.
// Residual accepted classes, both backstopped by the deadline
// clamp + per-sample re-latch exactly as the dyadic shadowing
// always is: a rising entry's compressed arc can still poke
// above a near candidate whose value sits above the launch point
// (the documented hump class), and a failure the neg-branch
// bound reads as unconstraining (gain - val <= 0 there) leaves
// Tclr at T0. Cost: one division, one sqrt and one ceil on the
// latch path (the mirror of Tshort's) plus ~3 mults/candidate
// for the two trees.
//
// A release leg cannot overshoot its target: with m1 = 0,
//   p - p1 = (1-tau)^2 * ((1+2*tau)*(p0-p1) + tau*T*m0)
//          <= (1-tau)^2 * (p1-p0) * (tau-1) <= 0
// for any m0 <= 3*delta. Mirrored for attacks (p1 < p0): the same
// identity gives p - p1 >= (1-tau)^3 * (p0 - p1) >= 0 for any
// m0 >= 3*delta -- a committed flat-landing attack leg never
// undershoots v1 (the hot-entry shorten leans on this at the FC
// boundary). Hence the release launch
// m0 = min(3*delta, max(0, dirPrev)): the same Fritsch-Carlson
// bound as the attack, mirrored -- a cap instead of a floor --
// plus the launch floor at 0.
//
// Attacks that fire mid-rise pick up the release leg's velocity
// as dirPrev -- the attack-side pickup, fed by the internal
// release. testSignal3 (upstream one-pole release) is kept only
// as an A/B reference against the upstream-release architecture.
//
// Cost: the audio hot path is dominated by the per-candidate
// scoring divisions and the argmin tree. The landing chord costs
// ONE shared division (the attack aim and the release endpoints
// are selected first); the ride adds one audio-rate division
// (rideMax); the shortened legs add one division + ceil (Tshort),
// its mirror (attTs), and one division + sqrt + ceil (Tclr), all
// on the latch path; the clearance check adds ~10 multiplies and
// two compares per candidate per sample plus its two trees. No
// state beyond the 7-wide loop, no delay lines beyond the bank's.
// nRel is slider-derived, so its guard runs in the control block.
// Latency: nAtt - 1.
//
// Release semantics: fixed DURATION, not fixed time constant --
// any rise, large or small, takes nRel samples from rest; a
// mid-flight re-latch that arrives hot keeps its velocity and
// lands early (T = ceil(3*gap/dirPrev) < nRel) instead of braking
// onto the fresh-leg cap. An isolated peak fully recovers in
// exactly nRel; a staircase of k window-min steps takes up to
// k*nRel. nRel = 1 gives instant rises to the window min.
//
// Known limitations, on purpose:
// * The next-deeper chain reads at dyadic resolution, so the true
//   second-deepest point can hide between scales (the
//   shadowed-second-deepest gap): the landing chord aims at the
//   nearest strictly-deeper point one scale out, not necessarily
//   the true one. nhV mirrors the same blind spot on the release
//   side.
// * The hump class: an attack launched off a rising tangent can
//   poke briefly above a flat-playing constraint before the
//   deadline-0 candidate clamps it. Rising tangents come from the
//   internal release legs, so that is where the humps come from.
//   Escape hatch if out <= grPlay must be bit-exact: min the
//   output with the delayed raw GR, at the price of a C1 corner
//   at the touch point.
// * A v1 that drops back to just above the gain mid-rise is
//   flown past rather than re-targeted (see relTrig): the
//   transient can arc above the momentary window min -- inside
//   the hump class, per-play-time deadlines still enforced --
//   before attNeed reels it in. Re-targeting down would trade
//   that arc for a velocity corner.
// * A ride into a pin that RECURS (nhV read high in the dyadic
//   blind spot, or the min value recurring past the largest
//   excluding suffix) parks at v1 instead of lifting -- as a
//   flat landing (the shortened leg decelerates in) rather than
//   a velocity chop.
//========================================================================

//========================================================================
// library part
// (The reference implementations slidingReducePair / slidingMinIdx that
// the bank's semantics are defined against live in hermiteSmoother.dsp.)
//========================================================================

//-------------------------`slidingMinIdxBankAtt`------------------------
// ONE pair cascade over the raw signal; the attack window and the taps
// are (delay, combine) reads of its stages, so the bank needs only nB
// cascade stages and no delay lines beyond the tap alignment.
//
// Scales, small to large: tap i = min over the NEXT pow2(i) samples
// (all taps share their trailing edge with the attack window's oldest
// sample = the one playing now), i = 0 .. nB-1; on top the attack
// window over nAtt (verbatim slidingMinIdx semantics). Every min rides
// with the EXACT play index of its oldest occurrence (deadline
// convention).
//
// The next-deeper chain's top link is the flat sentinel (v1, i1+1)
// unconditionally: with no lookahead beyond the window there is never a
// visible strictly-deeper point outside it, so landings on the
// full-window target land flat (zero chord).
//
// A tap whose window exceeds the current nAtt outputs (ma.MAX, 1): a
// value that can never become a binding constraint. Its window clipped
// to nAtt IS the attack window, so it enters the chain as (v1, i1),
// and equal values pass outward through it like any shared min.
//
// #### Usage
//
// ```
// _ : slidingMinIdxBankAtt(nAtt,maxAtt) : si.bus(4*(nB+1) + 2)
// ```
//
// * input: the RAW GR signal
// * `nAtt`: attack window length (1 <= nAtt <= maxAtt, may vary)
// * `maxAtt`: compile-time maximum (int)
// * out1..out4:            v1, i1, npFullV, npFullD (attack window)
// * out(5+4i)..out(8+4i):  tap i value, play idx, npV, npD
// * last two outs:         nhV, nhD -- the next-higher pair: the
//   ceiling level after the pinned min plays out, and the play index
//   of that level's own min, read off the mirror-aligned release taps
//   (see the header)
//----------------------------------------------------------------------
slidingMinIdxBankAtt(nAtt, maxAtt, x) = (v1, i1, npFullV, npFullD), par(i, nB, (outV(i), outD(i), npTV(i), npTD(i))), (nhV, nhD)
    with {
        nB = maxNrBits(maxAtt);
        // tap scales 2^0 .. 2^(nB-1)
        intMax = 2147483647;
        // play idx 0 = the sample playing now = the attack window's oldest
        idxFromOldest(tMin) = (nAtt-1)-(ba.time-tMin);
        minIdxOp(va, ta, vb, tb) = select2(pickSecond, va, vb), select2(pickSecond, ta, tb)
            with {
                pickSecond = (vb<va)|((vb==va)&((tb-ta)<0));
            };

        // THE shared cascade: pair i = (min, oldest timestamp) over the
        // last pow2(i) raw input samples
        casc = (x, ba.time):sequentialOperatorParOut(nB-1);
        cV(i) = casc:ba.selector(2*i, 2*nB);
        cT(i) = casc:ba.selector(2*i+1, 2*nB);

        // a sliding min+idx over the last m samples of x, read off the
        // shared stages: block i = stage i delayed into place, disabled
        // blocks read the identity pair; the fold is a balanced tree
        window(m, mMax, nBl) = par(i, nBl, ((cV(i), cT(i)):par(j, 2, _@sumPrevBlocks(m, mMax, i)):useVal(m, mMax, i))):combineTree(nBl);
        useVal(m, mMax, i) = select2(isUsed(m, mMax, i), ma.MAX, _), select2(isUsed(m, mMax, i), intMax, _);

        // attack window: the last nAtt samples of x (oldest = the sample
        // playing now)
        fullA = window(nAtt, maxAtt, nB);
        v1 = fullA:(_, !);
        i1 = fullA:(!, _):idxFromOldest;

        // tap path: cascade stage i covers [t-pow2(i)+1, t]; delaying the
        // pair by nAtt-pow2(i) moves that to the first pow2(i) samples to
        // play. The delayed timestamp then yields the exact play index
        // through the same idxFromOldest.
        dl(i) = de.delay(maxAtt-pow2(i), max(0, nAtt-pow2(i)));
        tV(i) = cV(i):dl(i);
        tD(i) = cT(i):dl(i):idxFromOldest;
        active(i) = pow2(i)<=nAtt;
        outV(i) = select2(active(i), ma.MAX, tV(i));
        outD(i) = select2(active(i), 1, tD(i));

        // --- the next-deeper chain --- (top link = the flat sentinel:
        // no lookahead beyond the window)
        // sV/sD: the tap clipped to the current window; a disabled tap's
        // clipped window IS the attack window
        sV(i) = select2(active(i), v1, tV(i));
        sD(i) = select2(active(i), i1, tD(i));
        npFullV = v1;
        npFullD = i1+1;
        // chain indexed from the top: k = 0 is the largest tap, whose
        // neighbour is the attack window; tap i sits at k = nB-1-i.
        npKV(0) = v1;
        npKD(0) = select2(v1<sV(nB-1), npFullD, i1);
        npKV(k) = select2(sV(nB-k)<sV(nB-1-k), npKV(k-1), sV(nB-k));
        npKD(k) = select2(sV(nB-k)<sV(nB-1-k), npKD(k-1), sD(nB-k));
        npTV(i) = npKV(nB-1-i);
        npTD(i) = npKD(nB-1-i);

        // --- release taps + next-higher chain ---
        // The mirror alignment: suffix j = the LAST pow2(j) samples of
        // the window (all suffixes share their leading edge with the
        // window's newest sample), read straight off the shared cascade
        // with NO delay: stage j at time t covers [t - 2^j + 1, t] =
        // play [nAtt - 2^j, nAtt - 1]. The attack taps pay nAtt - 2^i of
        // delay to slide every stage to the playing edge; the release
        // taps are the undelayed stages. A suffix wider than the window
        // would include samples that already played; exc() is
        // automatically false there (nAtt - 2^j < 0 <= i1), so no
        // separate active() gate is needed.
        //
        // Nested suffixes sample the ceiling schedule
        // C(s) = min over play [s, nAtt) at dyadic distances from the
        // far edge. nhV/nhD = the value of the LARGEST suffix that
        // excludes the pin i1 (= the level the ceiling lifts to when the
        // pinned min plays out, at dyadic resolution) and the play index
        // of that suffix's min. exc(j) is monotone (false above some
        // j*), so the chain -- later links override -- lands on j*.
        // Dips between i1 and the suffix start are invisible (nhV can
        // read HIGH: the mirror of the shadowed-second-deepest gap);
        // safe here because nhV only shapes the landing slope m1, which
        // keeps the flight <= v1 regardless (h11 <= 0), and post-lift
        // re-latches re-target on true data. If no suffix excludes the
        // pin (i1 = nAtt - 1), or the min value recurs later so a suffix
        // still contains it, nhV = v1: the flat sentinel, aim 0 --
        // mirroring npFull.
        exc(j) = (nAtt-pow2(j))>i1;
        nhKV(0) = select2(exc(0), v1, cV(0));
        nhKT(0) = select2(exc(0), 0, cT(0));
        nhKV(j) = select2(exc(j), nhKV(j-1), cV(j));
        nhKT(j) = select2(exc(j), nhKT(j-1), cT(j));
        nhV = nhKV(nB-1);
        nhD = select2(exc(0), i1+1, (nhKT(nB-1):idxFromOldest));

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

//---------------------`hermiteAttackReleaseFollower`--------------------
// Event-driven gain envelope: chases the critical constraint with
// latched Hermite attack legs; between attack legs it rises toward the
// window min v1 with Hermite release legs of nRel samples, and HOLDS
// while v1 == gain.
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
// * `cands`: 4*nC + 2 signals: (value, deadline, next-deeper value,
//   next-deeper deadline) per candidate, then the next-higher pair
//   (nhV, nhD) from the release taps. The deadline is the exact play
//   index; the next-deeper pair becomes the attack landing chord (own
//   value as next-deeper value = land flat); the next-higher pair
//   drives the lift-aware ride and is selected into the shared
//   landing chord for release legs, but the release LANDS FLAT (the
//   naive aim measured as a regression -- see the header). Candidate
//   0 MUST be the full attack window: its value doubles as the
//   release target v1 and its deadline as the pin's play index i1.
//   Disabled candidates read (ma.MAX, 1, _, _) and never win.
//
// Per sample, every candidate gets (requiredSlope, value, deadline,
// npVal, npDl) with requiredSlope = (value - gain)/deadline; the
// critical candidate is the argmin (steepest descent required), and
// its chord (npVal - value)/(npDl - deadline) is the landing slope.
//
// Triggers:
// * attack: critical-constraint re-latch with the creep gate, the
//   landing handoff, the clearance-checked launch, and the flat-chord
//   hot-entry shorten (see the header).
// * release: three-term trigger. v1 > p1 latches from rest
//   (gain == p1 there) and re-latches mid-leg whenever the window
//   min rises above the running target; a target that drops back is
//   flown past. Two ride terms re-plan every sample -- while a lift
//   is scheduled (liftAhead) and while a relaxed launch is latched
//   (the flown-whole guard) -- both gated on attNeed == 0, so
//   attacks always win. Inert mid-attack-leg (v1 <= p1 until
//   touchdown).
// * idle (arrived, v1 == gain) holds gain. The landed target stays
//   the window min through its own play sample, so peaks are held
//   through the peak's play sample.
//----------------------------------------------------------------------
hermiteAttackReleaseFollower(nC, nRel, cands) = (loop~si.bus(7)):(_, si.block(6))
    with {
        // release reads: candidate 0's value/deadline = the attack-window
        // min and the pin's play index; the tail = the next-higher pair
        v1 = cands:ba.selector(0, 4*nC+2);
        i1 = cands:ba.selector(1, 4*nC+2);
        nhV = cands:ba.selector(4*nC, 4*nC+2);
        nhD = cands:ba.selector(4*nC+1, 4*nC+2);

        // state: gain, p0, m0, p1, m1, k, T (previous-sample values inside loop)
        loop(gain, p0, m0, p1, m1, k, T) = gainN, p0N, m0N, p1N, m1N, kN, TN
            with {
                dirPrev = gain-gain';
                // current slope, units/sample

                // ---- critical-constraint selection ----
                trip(val, dl, npv, npd) = (val-gain)/max(1, dl), val, dl, npv, npd;
                scored = cands:(si.bus(4*nC), si.block(2)):par(i, nC, trip);
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
                // balanced AND fold for the clearance check (0/1
                // booleans; & is associative, tree shortens the chain)
                clrTree(1) = _;
                clrTree(2) = &;
                clrTree(N) = (clrTree(half), clrTree(N-half)):&
                    with {
                        half = int(N/2);
                    };
                // balanced min fold (the chop-class Tclr -- the
                // smallest failed deadline)
                minTree(1) = _;
                minTree(2) = min;
                minTree(N) = (minTree(half), minTree(N-half)):min
                    with {
                        half = int(N/2);
                    };
                // balanced min fold over ratios num/den carried as pairs
                // and compared cross-multiplied, all dens > 0 -- no
                // division on the tree (the zipper-class Tclr bound)
                minPair(na, da, nb, db) = select2(pk, na, nb), select2(pk, da, db)
                    with {
                        pk = (nb*da)<(na*db);
                    };
                mpTree(1) = si.bus(2);
                mpTree(2) = minPair;
                mpTree(N) = (mpTree(half), mpTree(N-half)):minPair
                    with {
                        half = int(N/2);
                    };

                // ---- triggers ----
                arrived = k>T;
                // previous leg done, at rest
                attNeed = critVal<gain;
                // re-latch when the critical value changes, when its (exact)
                // deadline undercuts the running leg's remaining time
                // (equal-depth peak that plays sooner: plateaus), or when at
                // rest (a release leg may have moved gain off p1, so the !=
                // test alone no longer suffices). On a steady leg all three
                // stay quiet.
                // EXCEPT: a changed minimum that plays at-or-after the
                // running leg's arrival and demands no steeper descent than
                // the leg has left is flown past, not re-latched -- the
                // creep gate. Chords are compared cross-multiplied (both
                // deadlines >= 0, so the direction is preserved and no
                // division lands on the loop's critical path; rRem = 0
                // makes steeper false, so a landed leg still holds its
                // target through the play sample and re-launches via
                // `arrived`). During a release leg p1 - gain > 0 while
                // critVal - gain < 0, so steeper is true and the creep gate
                // can never swallow an attack that fires mid-rise.
                rRem = max(0, T-k);
                // remaining steps of the leg
                steeper = ((critVal-gain)*rRem)<((p1-gain)*critDl);
                flyOn = (steeper==0)&(critDl>=rRem);
                // the landing handoff: a completed leg's velocity would
                // otherwise die in the play-hold sample -- the leg lands
                // (its last glide sample carries the landing step, the
                // chord aim or a deadline clamp), the next sample HOLDS
                // (dirPrev on the sample after reads 0), and only then
                // does `arrived` re-latch: every flown-whole joint would
                // restart from rest. On a descending schedule that restart
                // is a limit cycle (the ceiling zipper -- see the header).
                // When deeper work is pending, re-latch on the first
                // post-landing sample instead (k == T: the previous sample
                // produced tau = 1), where dirPrev IS the landing step --
                // the aim machinery finally hands its velocity to the next
                // leg. attNeed gates it: a landed WINDOW-DEEPEST pin reads
                // critVal == gain, so peak holds (and every release hold)
                // are untouched. Descending through a shallower pin's play
                // sample is safe: gain <= its value throughout.
                landed = k==T;
                attTrig = attNeed&(((critVal!=p1)&(flyOn==0))|(critDl<rRem)|arrived|landed);
                // release trigger, three terms:
                // 1) v1 > p1: latch from rest AND re-latch mid-leg whenever
                //    the window min rises above the running target (at rest
                //    gain == p1, so this reads v1 > gain; no trigger at
                //    v1 == gain: hold, which covers a landed attack target
                //    through its own play sample; the per-sample re-latch
                //    is what turns a creeping v1 into one governed rise --
                //    see the header for the micro-leg staircase it
                //    prevents). Implies v1 > gain, hence attNeed = 0, and
                //    is inert mid-attack-leg (v1 <= p1 until touchdown).
                // 2) liftAhead & (v1 > gain): while a strictly higher
                //    level is scheduled behind the pin, re-plan every
                //    sample so the ride cap (see m0t) stays inductively safe
                //    and its boost keeps feeding. Strict v1 > gain: a pinned
                //    gain must not latch (a flat latch would dip, m0 <= 0,
                //    below the pin).
                // 3) the flown-whole guard: a curve latched with a
                //    relaxed launch (m0 above the FC cap -- only the ride
                //    does this) must never fly uncorrected, since per-sample
                //    re-latch is what makes the ride safe. Detected from
                //    latched state alone: m0*T > 3*(p1 - p0), release legs
                //    only (p1 >= p0), with a 1e-8 relative margin so an
                //    FC-capped launch (m0 == 3*delta up to rounding) never
                //    trips it and block S-curves keep flying whole.
                // Terms 2 and 3 yield to attNeed explicitly: unlike term 1
                // they do not imply v1 > gain, and a missed attack is a
                // brickwall leak. A v1 that drops back under a flying
                // release target is still flown past; if the gain crosses
                // it, attNeed catches the crossing with a smooth arc.
                liftAhead = nhV>v1;
                relTrig = (v1>p1)|((attNeed==0)&((liftAhead&(v1>gain))|((p1>=p0)&((m0*T*0.99999999)>(3*(p1-p0))))));
                trig = attTrig|relTrig;

                // ---- new-segment values (only used when trig == 1) ----
                // momentum-preserving re-latch: a release re-latch
                // arriving with dirPrev above the fresh-leg FC cap
                // 3*(v1 - gain)/nRel would clamp the launch onto that cap
                // (m0 = min(3*delta, dirPrev)) -- a one-sample velocity
                // corner whenever a leg re-plans mid-flight (term 1 late in
                // a leg; term 2 the sample a lift blip appears). Instead
                // SHORTEN the leg: T = ceil(3*(v1 - gain)/dirPrev) raises
                // the FC bound to meet dirPrev, so the launch keeps its
                // velocity and decelerates smoothly into the target,
                // landing early. Both select2 branches agree at the
                // governor boundary (dirPrev == 3*gap/nRel gives
                // Tshort == nRel), launches from rest are untouched
                // (capped false at dirPrev ~ 0), and ceil keeps T integer
                // so tau reaches exactly 1 (a fractional T parks a landed
                // leg shy of its target and deadlocks the v1 > p1
                // trigger). relGap can go <= 0 here (these values compute
                // every sample; term 3 can fire at v1 == gain): the eps
                // floor keeps the idle division finite and max(1, ...)
                // catches the negative branch -- a T = 1 stop, identical
                // in output to an m0 = 0 clamp.
                relGap = v1-gain;
                capped = (dirPrev*nRel)>(3*relGap);
                Tshort = ceil((3*relGap)/max(ma.EPSILON, dirPrev));
                relT = select2(capped, nRel, max(1, min(nRel, Tshort)));
                // the same medicine, mirrored onto flat-chord attacks: a
                // leg whose landing chord is flat (the critical candidate
                // IS the window-deepest -- its next-deeper sentinel copies
                // its own value, an exact compare, no tolerance) and whose
                // entry arrives hotter than the full-deadline FC floor
                // (dirPrev * critDl < 3 * (critVal - gain),
                // cross-multiplied, both sides negative there) is
                // SHORTENED instead of clamped onto m0 = 3*delta:
                // attT = ceil(3*gap/dirPrev) puts the floor exactly ON
                // dirPrev, so the entry keeps its velocity, decelerates
                // smoothly, lands at v1 EARLY and holds flat through the
                // pin's play sample. Early landing at the window-deepest
                // is safe by construction (every candidate value is >= v1:
                // nothing binds until the pin plays out), and the mirrored
                // no-undershoot lemma holds at m0 = 3*delta with equality,
                // so gain >= v1 survives -- m0t*Tt lands exactly ON
                // 3*(p1 - p0), like Tshort. Cool entries and sloped chords
                // fly T = critDl untouched (both select2 branches agree at
                // the boundary), so block S-curves are unaffected; the
                // trigger algebra stays live throughout, so nothing is
                // blinded -- see the header. The eps floor keeps the idle
                // division finite when dirPrev reads >= 0 (that branch is
                // discarded by the attHot gate, which is false for any
                // dirPrev >= 0: hold and rising entries launch as before).
                attGap = critVal-gain;
                flatChord = critNpV==critVal;
                attHot = flatChord&((dirPrev*critDl)<(3*attGap));
                attTs = ceil((3*attGap)/min(0-ma.EPSILON, dirPrev));
                attT = select2(attHot, critDl, max(1, min(critDl, attTs)));
                T0 = max(1, select2(relTrig, attT, relT));
                p1t = select2(relTrig, critVal, v1);
                delta = (p1t-gain)/Tt;
                // average slope, sign = direction
                // landing chord, ONE shared division: every attack landing
                // aims at the nearest strictly-deeper point one scale out
                // (the critical candidate's np pair; own value as np value =
                // land flat). The release endpoints select the next-higher
                // pair, keeping the chord live for schedule-aware
                // experiments, but the release LANDS FLAT (m1t below): the
                // naive aim measured as a regression -- see the header.
                aimV2 = select2(relTrig, critNpV, nhV);
                aimD1 = select2(relTrig, critDl, i1);
                aimD2 = select2(relTrig, critNpD, nhD);
                aim = (aimV2-p1t)/max(1, aimD2-aimD1);
                // Fritsch-Carlson bound: a launch floor on attacks
                // (delta < 0), a launch cap on releases (delta > 0)
                lo = 3*delta;
                // the launch tangent is velocity-continuous in BOTH
                // directions, with the FC bound kept on the overshoot side
                // only -- the downside for attacks, the upside for
                // releases. dirPrev is a release leg's decaying slope when
                // an attack fires mid-rise (the attack-side pickup) and ~0
                // on launches from a hold. m0 <= 3*delta with m1 = 0 keeps a
                // release leg <= its target everywhere (see the header), so
                // gain <= v1 survives the whole leg; under per-sample
                // re-latch (a creeping v1) the same cap doubles as the
                // approach governor: velocity <= 3*(v1 - gain)/nRel, fast
                // when far, gentle when near. The downward direction is NOT
                // picked up on releases: the target v1 is the deepest point
                // in the total lookahead, so a negative dirPrev (an attack
                // chain landing ON its pin's play sample, or a hump's
                // deadline-0 clamp step, with the release firing on the very
                // next sample) would swoop the gain below the bottom --
                // gratuitous over-reduction, ~0.15*T*|dirPrev| deep. The
                // launch is floored at 0 instead; attacks keep the two-sided
                // pickup (their targets legitimately lie below).
                //
                // the lift-aware ride. The v1-chase brakes as the gap
                // closes -- its drive term 3*(v1 - gain)/T^2 vanishes --
                // which is wasted braking when the pin plays out before the
                // gain could reach it anyway. While a lift is scheduled
                // (liftAhead), the launch may instead keep the boosted chase
                // velocity: dirPrev plus the drive the farther level nhV
                // would add, (nhV - v1) * 3/nRel^2 per sample (control-rate
                // constant), capped by the fastest rate that cannot cross v1
                // before the lift, rideMax = (v1 - gain)/(i1 + 1). One step
                // at v <= rideMax leaves gain <= v1, and trigger term 2
                // re-plans every sample with fresh (v1, i1, nhV), so the
                // bound holds by induction; the flown-whole guard (term 3)
                // covers the moment liftAhead vanishes under a still-hot
                // launch. Two corners are accepted, both bounded: a pin that
                // RECURS at the wall (nhV read high in the dyadic blind
                // spot; or the i1 = 0 rounding, whose leak is <= 3*gap/T^2,
                // orders below the hump class) parks the ride at v1 with a
                // flat landing instead of a lift; and attacks that fire
                // mid-ride pick up the hotter dirPrev, growing their hump
                // within the documented class. max(aB, ride) keeps the ride
                // never slower than the plain chase (and discards a negative
                // rideMax the same way).
                rideK = 3.0/(float(nRel)*float(nRel));
                rideMax = (v1-gain)/(i1+1);
                ride = min(dirPrev+(nhV-v1)*rideK, rideMax);
                // launch floor: never descend toward the deepest point (a
                // two-sided launch can swoop below the window min -- see
                // the header).
                aB = min(lo, max(0, dirPrev));
                m0a = max(lo, dirPrev);
                // the clearance-checked launch. A slow entry -- rest, or
                // the tail of a decelerated leg -- against a DESCENDING
                // schedule plans a cubic whose early crawl is outrun by
                // the ceiling: the raw catches it within a few samples, a
                // deadline-forced clamp lands on the playing value, and
                // the cycle restarts (the zipper -- see the header). The
                // info to refuse that plan sits in the bank: evaluate the
                // planned cubic at EVERY candidate deadline and demand
                // p(dl) <= val -- the play-time constraint, exactly, at
                // the scales the bank resolves. Division-free: multiply
                // the Hermite basis through by T^3, with m0*T and m1*T
                // folded to max(3*(p1t - gain), dirPrev*T) and its m1
                // mirror so the check does not wait on delta (all factors
                // coerced to float first: d^3 overflows int32 at large
                // windows). Disabled candidates auto-pass (min(val, 2)
                // caps the ma.MAX sentinel and any GR is <= 1, so the
                // capped bound only ever relaxes a true one); deadlines
                // outside (0, T) auto-pass (dl >= T lands first and holds
                // at p1 <= val; dl <= 0 is the sample playing now).
                //
                // On failure the leg is SHORTENED, velocity kept -- the
                // shorten medicine, third verse. (Launching at
                // max(lo, critScore), the argmin's own score, instead
                // would be a one-sample velocity corner at every
                // engagement -- rising entries CHOPPED onto the near-flat
                // chord; rest entries snapped from 0 to the chord.)
                // Shortening is the safe direction: at fixed play time t,
                // dp/dT = 2*tau*(1-tau)*t*(m0 - 3*delta)/T >= 0 since
                // m0 >= 3*delta = lo by construction in every regime, so
                // a shorter leg only ever lowers the path -- passing
                // candidates stay clear, only the failed set needs
                // covering. Two regimes, split on the entry direction:
                // * dirPrev > 0 (the chop class): land EARLY.
                //   Tclr = the smallest FAILED deadline: every failed
                //   candidate moves into the dl >= T auto-pass (the leg
                //   sits landed at p1 by then, and every candidate due
                //   at-or-before critDl has val >= critVal --
                //   deeper-and-sooner would have won the argmin), every
                //   passing one stays clear by monotonicity. Exact and
                //   division-free (minTree over select2(pass, dl, 1e30)).
                //   The turnaround arc a rising entry needs to stay C1
                //   still flies, compressed into the shortened leg (its
                //   peak scales with T), instead of being chopped flat.
                // * dirPrev <= 0 (the zipper class): let CURVATURE do
                //   it. The m0 = 0 cubic bounds every m0 <= 0 plan from
                //   above, and q(u) = 3u^2 - 2u^3 >= u^2 gives the
                //   sufficient condition tau >= sqrt(s) with
                //   s = (gain - val)/(gain - p1t), hence
                //   Tclr^2 = (gain - p1t) * minFailed(dl^2/(gain - val)),
                //   the min run cross-multiplied as a (num, den) pair
                //   tree: ONE division + ONE sqrt total, ceil'd integer
                //   like Tshort. Overshortens by at most sqrt(3) in the
                //   small-gap limit -- the safe direction. A failed
                //   candidate always has gain > val > p1t here (val > p1t
                //   by the argmin, val < gain since p(dl) <= gain for
                //   m0 <= 0), so s lands in (0, 1] and the den floor is
                //   idle; a hump-class failure (val >= gain, only
                //   reachable with m0 > 0) reads unconstraining in this
                //   branch by the same floor -- the chop class owns it.
                // Per-sample re-latching and the deadline clamp remain
                // the backstop for what the dyadic argmins shadow. Cost:
                // ~10 multiplies and two compares per candidate plus ~3
                // more for the two trees, one division, one sqrt, one
                // ceil; no state.
                ckT = 1.0*T0;
                ckT3 = ckT*ckT*ckT;
                m0aT = max(3*(p1t-gain), dirPrev*ckT);
                m1tT = select2(relTrig, max(3*(p1t-gain), min(0, aim)*ckT), 0);
                clearOne(val, dl, npv, npd) = pass, num, den, dlP
                    with {
                        df = 1.0*dl;
                        df2 = df*df;
                        df3 = df2*df;
                        valC = min(val, 2.0);
                        pl = gain*(2*df3-3*df2*ckT+ckT3)+m0aT*(df*(ckT-df)*(ckT-df))+p1t*(df2*(3*ckT-2*df))+m1tT*(df2*(df-ckT));
                        pass = (dl<=0)|(df>=ckT)|(pl<=(valC*ckT3));
                        num = select2(pass, df2, 1e30);
                        den = select2(pass, max(1e-30, gain-valC), 1.0);
                        dlP = select2(pass, df, 1e30);
                    };
                checks = cands:(si.bus(4*nC), si.block(2)):par(i, nC, clearOne);
                clear = checks:par(i, nC, (_, !, !, !)):clrTree(nC);
                clrPair = checks:par(i, nC, (!, _, _, !)):mpTree(nC);
                Tneg = ceil(sqrt(max(0.0, (gain-p1t)*(clrPair:(_, !))/(clrPair:(!, _)))));
                Tpos = checks:par(i, nC, (!, !, !, _)):minTree(nC);
                Tclr = select2(dirPrev>0, Tneg, Tpos);
                Tt = select2((relTrig==0)&(clear==0), T0, max(1, min(T0, Tclr)));
                m0t = select2(relTrig,
                    m0a,
                    select2(liftAhead, aB, max(aB, ride)));
                m1t = select2(relTrig, max(lo, min(0, aim)), 0);

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
                // at rest the output HOLDS: v1 == gain there (v1 > gain fires
                // a release leg, v1 < gain an attack); rises are latched legs
                gainN = select2(gliding, gain, hermiteVal);
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
// * `nAtt`: attack lookahead in samples (1 <= nAtt <= maxAtt, may
//   vary at control rate)
// * `nRel`: release leg length in samples (>= 1, may vary at control
//   rate); 1 = instant rises to the window min
// * `maxAtt`: compile-time maximum attack window (int)
//
// Latency: nAtt - 1 samples; delay the raw GR (and the audio in a full
// limiter) by the same amount to line up with the output.
//----------------------------------------------------------------------
lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR) = hermiteAttackReleaseFollower(nB+1, nRel, cands)
    with {
        nB = int(floor(log(maxAtt)/log(2))+1);
        // the bank output is the follower's candidate list: (value,
        // deadline, npV, npD) for the attack window, then for every tap,
        // then the next-higher pair (nhV, nhD) from the release taps
        cands = rawGR:slidingMinIdxBankAtt(nAtt, maxAtt);
    };

//-------------------------------- demo ---------------------------------
// out1: delayed raw GR (the constraint the smoother must stay <= )
// out2: smoother output
//
// Brickwall check: out2 <= out1 at every sample, up to the hump class
// (no hard clamp, so any violation measures that residual plus the
// shadowed-peak gap). Rises are S-curves toward the window min: gain
// holds through each peak's play sample, then recovers in nRel.
// release = 0 gives instant rises to the window min.

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
testSignal3 = testSignal1:relFollow
    with {
        relCoef = exp(-1.0/(testRelMs*0.001*ma.SR));
        relFollow(x) = loop~_
            with {
                loop(y) = min(x, x+(y-x)*relCoef);
            };
    };

// --- Smoother parameters ---
// compile-time maximum: 50 ms at maxSR. Lower maxSR if you never run
// above 48/96k, to save memory and a few reduce stages.
maxSR = 192000;
maxAtt = int(0.05*maxSR);

attMs = SmootherGroup(hslider("[0]attack lookahead [unit:ms]", 25, 0, 50, 0.1));
nAtt = max(2, min(maxAtt, int(attMs*0.001*ma.SR)));
relMs = SmootherGroup(hslider("[1]release [unit:ms]", 50, 0, 500, 0.1));
nRel = max(1, int(relMs*0.001*ma.SR));

process = MainGroup(demo(testSignal))
    with {
        demo(rawGR) = grPlay, smoothed
            with {
                grPlay = de.delay(maxAtt-1, nAtt-1, rawGR);
                smoothed = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR);
            };
    };
