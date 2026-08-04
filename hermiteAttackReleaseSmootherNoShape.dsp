declare name "hermiteAttackReleaseSmoother";
declare version "1.9.0";
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
// regressions on the noisy workload, so they ship NEUTRALIZED:
// the release chord (nhV - p1t)/(nhD - i1) is never computed, and
// its would-be division is branch-shared with the ride cap (see
// the latch block to restore it). Why more landing information
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
// term 3 stays quiet on these launches. Cost: shares the
// shortened-leg division + ceil with attTs (branch-exclusive by
// relTrig), so the pair costs ONE audio-rate division + ceil.
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
// two removed here. Cost: shares the shortened-leg division +
// ceil with Tshort (branch-exclusive by relTrig) -- the exact
// mirror, on the same divider.
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
// (handoff) plus ~5 multiplies and two compares per candidate
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
// with the m0*T and m1*T products directly -- the same x T form
// the latch itself now carries): the plan refuses, at the earliest possible
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
// release.
//
// Cost: the audio hot path is dominated by the argmin tree and
// the clearance check; per-candidate scoring is DIVISION-FREE
// (slopes ride the tree as cross-multiplied (num, den) pairs,
// dens >= 1). The landing chord and the ride cap share ONE
// division (the numerator/denominator pairs are selected first,
// branch-exclusive by relTrig); the shortened legs share ONE
// division + ceil (Tshort and attTs, same exclusivity); Tclr adds
// one division + sqrt + ceil; with tau that is FOUR audio-rate
// divisions total -- the tangent states are carried pre-multiplied
// by the leg length (m0T, m1T), keeping the per-latch delta
// division off the feedback critical path and two Hermite
// multiplies out of the evaluator. The clearance check adds ~5
// multiplies and two compares per candidate per sample (the cubic
// in Horner form, coefficients shared per sample) plus its two
// trees -- the all-pass AND tree is folded into the Tpos min tree
// (all-pass reads back as exactly 1e30). No
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
//========================================================================

//-------------------------`slidingMinIdxBankAtt`------------------------
// ONE pair cascade over the raw signal; the attack window and the taps
// are (delay, combine) reads of its stages, so the bank needs only nB
// cascade stages and no delay lines beyond the tap alignment.
//
// Scales, small to large: tap i = min over the NEXT pow2(i) samples
// (all taps share their trailing edge with the attack window's oldest
// sample = the one playing now), i = 0 .. nB-1; on top the attack
// window over nAtt. Every min rides
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
slidingMinIdxBankAtt(nAtt, maxAtt, x) = casc// (v0,t0, v1,t1, ... ) -- ONE instance
:ro.interleave(2, nB)// -> V(0..nB-1), T(0..nB-1)
:deal// -> Vw,Tw | Vt,Tt | Vn,Tn
:(winBlk, tapBlk, si.bus(2*nB))// -> v1,i1 | tV,tD | Vn,Tn
:spread// -> heads | tap units | np seed | nh
:(si.bus(4), par(i, nB, tapUnit(i)), si.bus(6+3*nB)):zipNp// -> heads | outV/outD | np chain in | nh
:(si.bus(4+2*nB), npChain, si.bus(2+3*nB)):(si.bus(4+4*nB), nhChain):lace// -> the declared output order
    with {
        nB = maxNrBits(maxAtt);
        // tap scales 2^0 .. 2^(nB-1)
        // play idx 0 = the sample playing now = the attack window's oldest.
        // (nAtt-1)-(ba.time-tMin) regrouped so the ba.time part is shared
        // across every call site; int add is associative, so bit-identical
        // even at wraparound.
        idxBase = (nAtt-1)-ba.time;
        idxFromOldest(tMin) = idxBase+tMin;
        minIdxOp(va, ta, vb, tb) = select2(pickSecond, va, vb), select2(pickSecond, ta, tb)
            with {
                pickSecond = (vb<va)|((vb==va)&((tb-ta)<0));
            };

        // THE shared cascade: pair i = (min, oldest timestamp) over the
        // last pow2(i) raw input samples. Instantiated exactly once; every
        // consumer below is reached by routing, never by re-selecting.
        casc = (x, ba.time):sequentialOperatorParOut(nB-1);

        // ro.interleave(2,nB) transposes the (v,t) pairs into lane order.
        // Each lane then feeds three consumers -- the attack window, the
        // tap path and the next-higher chain -- so triple both and zip the
        // copies back into (V,T) pairs per consumer.
        deal = ((si.bus(nB)<:si.bus(3*nB)), (si.bus(nB)<:si.bus(3*nB))):route(6*nB, 6*nB, (par(g, 3, par(i, nB, (g*nB+i+1, 2*g*nB+i+1), (3*nB+g*nB+i+1, 2*g*nB+nB+i+1)))));

        // attack window: the last nAtt samples of x (oldest = the sample
        // playing now), read sparse-table style: min is idempotent, so
        // TWO overlapping dyadic blocks that COVER the window give the
        // same (min, oldest-timestamp) pair as an exact partition --
        // minIdxOp on equal values passes the older timestamp, and the
        // oldest occurrence of the window min lies inside at least one
        // block, where it is also that block's oldest occurrence.
        //   suffix block: stage jW undelayed,      covers [t-2^jW+1, t]
        //   prefix block: stage jW delayed by dW,  covers [t-nAtt+1, t-nAtt+2^jW]
        // with jW = the largest j s.t. 2^jW <= nAtt, dW = nAtt-2^jW
        // (so dW < 2^jW and both blocks sit inside the window; nAtt = 1
        // makes the blocks coincide and the tie-break passes them
        // through). jW/dW depend only on nAtt: control-rate, and the sum
        // of comparator flags is integer-exact where a float log2 is not.
        jW = (par(i, nB, (pow2(i)<=nAtt)):>_)-1;
        dW = nAtt-(1<<jW);
        // dW never exceeds max(2^(nB-2)-1, maxAtt-2^(nB-1)): for
        // jW <= nB-2, dW < 2^jW <= 2^(nB-2); for jW = nB-1,
        // dW <= maxAtt-2^(nB-1). Compile-time int, so the two block
        // delay lines size to it instead of to 2^(nB-1) -- half the
        // added memory at the default maxAtt.
        dMax = max(0, max(pow2(max(0, nB-2))-1, maxAtt-pow2(nB-1)));
        // (V,T) -> (v1, i1)
        winBlk = (ba.selectn(nB, jW), ba.selectn(nB, jW))// wV, wT
        :si.bus(2)<:(si.bus(2), par(k, 2, de.delay(dMax, dW))):minIdxOp:_, idxFromOldest;

        // tap path: cascade stage i covers [t-pow2(i)+1, t]; delaying the
        // pair by nAtt-pow2(i) moves that to the first pow2(i) samples to
        // play. The delayed timestamp then yields the exact play index
        // through the same idxFromOldest.
        dl(i) = de.delay(maxAtt-pow2(i), max(0, nAtt-pow2(i)));
        active(i) = pow2(i)<=nAtt;
        tapBlk = par(i, nB, dl(i)), par(i, nB, dl(i):idxFromOldest);
        // -> tV, tD

        // v1/i1 feed a lot of sinks, so fan them out here and lay the bus
        // out consumer by consumer. i1+1 (= npFullD) is computed once and
        // split three ways: the head output, the np seed, and nhD.
        //   copies: v1  -> head, npFullV, one per tap unit, 2 for the np
        //                  seed, 1 for the nh seed
        //           i1  -> head, the +1, one per tap unit (sD), np seed,
        //                  one per nh link (exc)
        spread = ((_<:si.bus(nB+5)), (_<:si.bus(2*nB+3)), si.bus(4*nB)):(si.bus(nB+5), (_, (+(1)<:si.bus(3)), si.bus(2*nB+1)), si.bus(4*nB)):route(7*nB+10, 7*nB+10, (// heads: v1, i1, npFullV, npFullD
        (1, 1), (nB+6, 2), (2, 3), (nB+7, 4), // per tap unit i: v1, i1, tV(i), tD(i)
        par(i, nB, (3+i, 5+4*i), (nB+10+i, 6+4*i), (3*nB+11+i, 7+4*i), (4*nB+11+i, 8+4*i)), // np seed: v1 (as npKV(0)), v1 (compare), i1, npFullD
        (nB+3, 5+4*nB), (nB+4, 6+4*nB), (2*nB+10, 7+4*nB), (nB+8, 8+4*nB), // nh: v1, i1+1, then (i1, V, T) per link
        (nB+5, 9+4*nB), (nB+9, 10+4*nB), par(j, nB, (2*nB+11+j, 11+4*nB+3*j), (5*nB+11+j, 12+4*nB+3*j), (6*nB+11+j, 13+4*nB+3*j))));

        // one tap: the bank's own output pair, plus the chain's view of it
        // (sV/sD = the tap clipped to the current window; a disabled tap's
        // clipped window IS the attack window). sV is emitted twice: each
        // link of the np chain looks at its own tap and peeks at the next.
        tapUnit(i, v1i, i1i, tv, td) = select2(active(i), ma.MAX, tv), // outV(i)
        select2(active(i), 1, td), // outD(i)
        (select2(active(i), v1i, tv)<:si.bus(2)), // sV(i), twice
        select2(active(i), i1i, td);
        // sD(i)

        // --- the next-deeper chain --- (top link = the flat sentinel:
        // no lookahead beyond the window)
        // Walked from the largest tap down, emitting every intermediate,
        // so the whole chain costs one pass instead of one chain per tap.
        // Link m consumes sV(m+1), sD(m+1) and peeks at sV(m); the seed
        // (m = nB-1) peeks at sV(nB-1). sV(0)/sD(0) are dead ends and get
        // dropped by the route.
        zipNp = route(8*nB+10, 8*nB+8, (par(i, 4, (i+1, i+1)), par(i, nB, (5+5*i, 5+2*i), (6+5*i, 6+2*i)), // outV, outD
        par(i, 4, (5+5*nB+i, 5+2*nB+i)), // np seed
        (5*nB+2, 2*nB+9), // sV(nB-1)
        par(q, nB-1, (5*nB-5*q+3, 10+2*nB+3*q), // sV(nB-1-q)
        (5*nB-5*q+4, 11+2*nB+3*q), // sD(nB-1-q)
        (5*nB-5*q-3, 12+2*nB+3*q)), // sV(nB-2-q)
        par(i, 2+3*nB, (9+5*nB+i, 7+5*nB+i))));
        // nh, passed on
        npChain = (npSeed, si.bus(3*(nB-1))):seq(q, nB-1, (si.bus(2*q), npStep, si.bus(3*(nB-2-q))));
        npSeed(vpv, vc, i1s, nfd, svTop) = vpv, select2(vc<svTop, nfd, i1s);
        npStep(pv, pd, svn, sdn, svm) = pv, pd, // emit link m+1
        select2(c, pv, svn), select2(c, pd, sdn)// carry link m
            with {
                c = svn<svm;
            };

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
        // exc(0) is carried down the chain because nhD needs it again.
        nhChain = (nhSeed, si.bus(3*(nB-1))):seq(j, nB-1, (nhStep(j+1), si.bus(3*(nB-2-j)))):nhFin;
        exc(j, i1j) = (nAtt-pow2(j))>i1j;
        nhSeed(v1n, i1p1, i1j, vj, tj) = e0, i1p1, select2(e0, v1n, vj), select2(e0, 0, tj)
            with {
                e0 = exc(0, i1j);
            };
        nhStep(j, e0, i1p1, kv, kt, i1j, vj, tj) = e0, i1p1, select2(ej, kv, vj), select2(ej, kt, tj)
            with {
                ej = exc(j, i1j);
            };
        nhFin(e0, i1p1, kv, kt) = kv, select2(e0, i1p1, idxFromOldest(kt));

        // interleave the four per-tap lanes back into the declared shape:
        // (outV, outD, npTV, npTD) per tap. The np chain emitted its links
        // largest-tap-first, so it is read back in reverse.
        lace = route(4*nB+6, 4*nB+6, (par(i, 4, (i+1, i+1)), par(i, nB, (5+2*i, 5+4*i), (6+2*i, 6+4*i), (3+4*nB-2*i, 7+4*i), (4+4*nB-2*i, 8+4*i)), (5+4*nB, 5+4*nB), (6+4*nB, 6+4*nB)));

        // shared helpers (op bound to minIdxOp):
        sequentialOperatorParOut(N) = seq(i, N, operator(i));
        operator(i) = si.bus(2*i), (si.bus(2)<:(si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))):minIdxOp)));
        maxNrBits(m) = int2nrOfBits(m);
        pow2(i) = 1<<i;
        int2nrOfBits(v) = int(floor(log(v)/log(2))+1);
    };

//----------------------------`slidingMinIdx`----------------------------
// (min value, play index of its FIRST occurrence) over the last n
// samples of x: the bank's window read -- one (value, timestamp)
// pair cascade + two overlapping dyadic blocks that COVER the
// window + oldest-wins tie-break -- without the taps and chains.
// Play idx 0 = the window's OLDEST sample; align the window so its
// oldest sample is the one playing now and idx IS the distance in
// samples to the event.
//
// #### Usage
//
// ```
// _ : slidingMinIdx(n, maxN) : _, _
// ```
//
// * `n`: window length (1 <= n <= maxN, may vary at control rate)
// * `maxN`: compile-time maximum (int)
// * out: minVal, playIdx (0 .. n-1)
//----------------------------------------------------------------------
slidingMinIdx(n, maxN, x) = casc:deint:reduce:win
    with {
        nB = int(floor(log(maxN)/log(2))+1);
        pow2(i) = 1<<i;
        // (n-1)-(ba.time-tMin) regrouped as in the bank: int add is
        // associative, so bit-identical even at wraparound
        idxFromOldest(tMin) = ((n-1)-ba.time)+tMin;
        minIdxOp(va, ta, vb, tb) = select2(pickSecond, va, vb), select2(pickSecond, ta, tb)
            with {
                pickSecond = (vb<va)|((vb==va)&((tb-ta)<0));
            };
        operator(i) = si.bus(2*i), (si.bus(2)<:(si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))):minIdxOp)));
        // the one and only cascade: (v0,t0, v1,t1, ..., v(nB-1),t(nB-1))
        casc = (x, ba.time):seq(i, nB-1, operator(i));
        // transpose the (v,t) pairs into lane order: v(0..nB-1), t(0..nB-1)
        deint = ro.interleave(2, nB);
        // two overlapping dyadic blocks cover the window (min is
        // idempotent; minIdxOp on equal values passes the OLDER
        // timestamp) -- see the bank's window read for the full
        // argument, incl. the dMax sizing bound
        jW = (par(i, nB, (pow2(i)<=n)):>_)-1;
        dW = n-(1<<jW);
        reduce = ba.selectn(nB, jW), ba.selectn(nB, jW);
        // -> wV, wT
        dMax = max(0, max(pow2(max(0, nB-2))-1, maxN-pow2(nB-1)));
        win = si.bus(2)<:(si.bus(2), par(k, 2, de.delay(dMax, dW))):minIdxOp:_, idxFromOldest;
        // -> v, idx
    };

//------------------------`slidingMinIdxBankDJ`--------------------------
// The DJ glide's demand ladder (v1.9.0): the slidingMinIdx window read
// PLUS the dyadic prefix taps of the same window -- tap j = min over
// the FIRST pow2(j) samples to play, the bank's attack-tap alignment
// (delay n - 2^j on cascade stage j) reused over the DJ lookahead
// window. ONE (value, timestamp) pair cascade feeds the window read;
// the taps ride its VALUE lane only -- they carry no timestamps,
// because the consumer plans each rung against its window EDGE, not
// the min's exact position (see the loop comment). The taps sample
// the ceiling schedule C(s) = min over play [0, s) at dyadic
// horizons -- the pending-demand structure the single window min
// throws away.
//
// A tap wider than the window reads ma.MAX: its glide target
// min(0, ma.MAX + os) pins to 0, which the strict gate
// (tgt < yState <= 0) reads as inert -- no separate active() gate at
// the consumer.
//
// #### Usage
//
// ```
// _ : slidingMinIdxBankDJ(n, maxN) : si.bus(nB+2)
// ```
//
// * `n`: window length (1 <= n <= maxN, may vary at control rate)
// * `maxN`: compile-time maximum (int); nB = int2nrOfBits(maxN)
// * out1, out2: v, idx (the full window, == slidingMinIdx)
// * out(3+j):   tap j value
//----------------------------------------------------------------------
slidingMinIdxBankDJ(n, maxN, x) = casc:fanout:reduce:(win, taps)
    with {
        nB = int(floor(log(maxN)/log(2))+1);
        pow2(i) = 1<<i;
        idxFromOldest(tMin) = ((n-1)-ba.time)+tMin;
        minIdxOp(va, ta, vb, tb) = select2(pickSecond, va, vb), select2(pickSecond, ta, tb)
            with {
                pickSecond = (vb<va)|((vb==va)&((tb-ta)<0));
            };
        operator(i) = si.bus(2*i), (si.bus(2)<:(si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))):minIdxOp)));
        // the one and only cascade: (v0,t0, v1,t1, ..., v(nB-1),t(nB-1))
        casc = (x, ba.time):seq(i, nB-1, operator(i));

        // deinterleave the (v,t) pairs into lane order, duplicate the value
        // lane (window reduction and tap path each need one copy), then park
        // the tap copy at the end:
        //   -> vA(0..nB-1), t(0..nB-1), vB(0..nB-1)
        fanout = ro.interleave(2, nB):(si.bus(nB)<:si.bus(2*nB)), si.bus(nB):si.bus(nB), ro.crossNM(nB, nB);
        // -> wV, wT, vB(0..nB-1)
        reduce = ba.selectn(nB, jW), ba.selectn(nB, jW), si.bus(nB);

        jW = (par(i, nB, (pow2(i)<=n)):>_)-1;
        dW = n-(1<<jW);
        dMax = max(0, max(pow2(max(0, nB-2))-1, maxN-pow2(nB-1)));
        // (wV,wT) -> (v, idx)
        win = si.bus(2)<:(si.bus(2), par(k, 2, de.delay(dMax, dW))):minIdxOp:_, idxFromOldest;

        // tap path, the bank's alignment verbatim: stage j covers
        // [t-2^j+1, t]; delayed by n-2^j it covers the first 2^j
        // samples to play. Value lane only: the consumer's edge
        // deadline needs no timestamp.
        dl(j) = de.delay(maxN-pow2(j), max(0, n-pow2(j)));
        active(j) = pow2(j)<=n;
        taps = par(j, nB, dl(j):select2(active(j), ma.MAX));
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
// hermiteAttackReleaseFollower(nC, nRel, checkEvery, cands) : _
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
// Per sample, every candidate gets (num, den, value, deadline,
// npVal, npDl) with num/den = (value - gain)/max(1, deadline), the
// required slope carried UNDIVIDED; the critical candidate is the
// argmin (steepest descent required), compared cross-multiplied
// (dens >= 1 > 0 preserve direction and the leftmost tie-break),
// and its chord (npVal - value)/(npDl - deadline) is the landing
// slope.
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
hermiteAttackReleaseFollower(nC, nRel, checkEvery, cands) = (loop~si.bus(7)):(_, si.block(6))
    with {
        // release reads: candidate 0's value/deadline = the attack-window
        // min and the pin's play index; the tail = the next-higher pair
        v1 = cands:ba.selector(0, 4*nC+2);
        i1 = cands:ba.selector(1, 4*nC+2);
        nhV = cands:ba.selector(4*nC, 4*nC+2);
        nhD = cands:ba.selector(4*nC+1, 4*nC+2);

        // state: gain, p0, m0T, p1, m1T, k, T (previous-sample values
        // inside loop). The tangent states are carried PRE-MULTIPLIED by
        // the leg length (m0T = m0*T, m1T = m1*T): the Hermite evaluator
        // only ever reads the products (h10*T*m0, h11*T*m1), so latching
        // the product keeps the per-latch delta division off the
        // feedback loop's critical path and two multiplies per sample
        // out of the evaluator. min/max/select2 all commute bitwise with
        // scaling by the (positive) leg length, so each latch branch is
        // the plain-slope branch's value*T bit-for-bit, and a capped
        // launch stores its bound exactly rather than a divide-then-
        // remultiply ulp off it -- see g3 below.
        loop(gain, p0, m0T, p1, m1T, k, T) = gainN, p0N, m0TN, p1N, m1TN, kN, TN
            with {
                dirPrev = gain-gain';
                // current slope, units/sample

                // ---- critical-constraint selection ----
                // score = (val - gain)/max(1, dl), carried as a (num, den)
                // pair and compared cross-multiplied inside the tree: dens
                // are >= 1 > 0, so the direction is preserved, ties still
                // keep the left (sooner) candidate, and NO division runs
                // per candidate -- nC up-front divides would be the hot
                // path's single biggest cost. The disabled-tap sentinel
                // (ma.MAX) stays correctly ordered even where a cross
                // product saturates (inf compares on the right side).
                trip(val, dl, npv, npd) = val-gain, max(1, dl), val, dl, npv, npd;
                scored = cands:(si.bus(4*nC), si.block(2)):par(i, nC, trip);
                crit = scored:red6(nC);
                // the winner's (val - gain) comes out of the tree for
                // free: reading it spares recomputing the subtraction at
                // attNeed/steeper/attGap, and (critNum < 0) ==
                // (critVal < gain) exactly in IEEE (a nonzero difference
                // of two doubles never rounds to zero).
                critNum = crit:(_, !, !, !, !, !);
                critVal = crit:(!, !, _, !, !, !);
                critDl = crit:(!, !, !, _, !, !);
                critNpV = crit:(!, !, !, !, _, !);
                critNpD = crit:(!, !, !, !, !, _);
                amin6(na, da, va, ta, ua, wa, nb, db, vb, tb, ub, wb) = select2(pk, na, nb), select2(pk, da, db), select2(pk, va, vb), select2(pk, ta, tb), select2(pk, ua, ub), select2(pk, wa, wb)
                    with {
                        pk = (nb*da)<(na*db);
                    };
                // balanced tree: leftmost-min selection is associative, so the
                // winner matches the sequential fold, with a dependency chain
                // of ceil(log2(nC)) instead of nC-1 selects -- and this chain
                // sits on the feedback loop's critical path (two multiplies
                // per node instead of a division per candidate).
                red6(1) = si.bus(6);
                red6(2) = amin6;
                red6(N) = (red6(half), red6(N-half)):amin6
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
                attNeed = critNum<0;
                // re-latch when the critical value changes, when its (exact)
                // deadline undercuts the running leg's remaining time
                // (equal-depth peak that plays sooner: plateaus), or when at
                // rest (a release leg may have moved gain off p1, so the !=
                // test alone does not suffice). On a steady leg all three
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
                steeper = (critNum*rRem)<((p1-gain)*critDl);
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
                // arrived (k > T, previous leg done, at rest) and landed
                // (k == T, first post-landing sample) enter attTrig only
                // as their union: k >= T, one compare
                attTrig = attNeed&(((critVal!=p1)&(flyOn==0))|(critDl<rRem)|(k>=T));
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
                //    sample so the ride cap (see m0Tt) stays inductively safe
                //    and its boost keeps feeding. Strict v1 > gain: a pinned
                //    gain must not latch (a flat latch would dip, m0 <= 0,
                //    below the pin).
                // 3) the flown-whole guard: a curve latched with a
                //    relaxed launch (m0 above the FC cap -- only the ride
                //    does this) must never fly uncorrected, since per-sample
                //    re-latch is what makes the ride safe. Detected from
                //    latched state alone: m0T > 3*(p1 - p0), release legs
                //    only (p1 >= p0), with a 1e-8 relative margin so an
                //    FC-capped launch (m0T == 3*(p1 - p0), exactly, since
                //    the product itself is latched) never trips it and block
                //    S-curves keep flying whole.
                // Terms 2 and 3 yield to attNeed explicitly: unlike term 1
                // they do not imply v1 > gain, and a missed attack is a
                // brickwall leak. A v1 that drops back under a flying
                // release target is still flown past; if the gain crosses
                // it, attNeed catches the crossing with a smooth arc.
                liftAhead = nhV>v1;
                relTrig = (v1>p1)|((attNeed==0)&((liftAhead&(v1>gain))|((p1>=p0)&((m0T*0.99999999)>(3*(p1-p0))))));
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
                // ONE shared division + ceil serves BOTH shortened legs:
                // Tshort (release, the max(eps, ...) branch) and attTs
                // (attack, the min(-eps, ...) branch) are consumed on
                // opposite sides of relTrig -- relT only through T0's
                // release branch, attT only through its attack branch --
                // so the shared quotient reproduces whichever one is
                // read, bit-exactly, and the discarded branch's value
                // (always finite: |den| >= eps) is never consumed.
                shGap = select2(relTrig, attGap, relGap);
                shDir = select2(relTrig, min(0-ma.EPSILON, dirPrev), max(ma.EPSILON, dirPrev));
                // g3 == 3*(p1t - gain), bitwise: subtracting gain commutes
                // through select2 (shGap IS p1t - gain branch by branch),
                // and it feeds Tsh, the FC latch caps and the clearance
                // coefficients from ONE multiply. The x T latch earns its
                // keep here: a slope-form latch (m0 = 3*(p1t - gain)/Tt,
                // read back as m0*T) would land an ulp off the cap; the
                // x T latch stores g3 itself, so a capped launch sits
                // exactly ON 3*(p1 - p0) and the flown-whole margin
                // guards only the ride entries.
                g3 = 3*shGap;
                Tsh = ceil(g3/shDir);
                relT = select2(capped, nRel, max(1, min(nRel, Tsh)));
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
                // no-undershoot lemma holds at the FC boundary with equality,
                // so gain >= v1 survives -- m0Tt lands exactly ON
                // 3*(p1 - p0), like Tshort. Cool entries and sloped chords
                // fly T = critDl untouched (both select2 branches agree at
                // the boundary), so block S-curves are unaffected; the
                // trigger algebra stays live throughout, so nothing is
                // blinded -- see the header. The eps floor keeps the idle
                // division finite when dirPrev reads >= 0 (that branch is
                // discarded by the attHot gate, which is false for any
                // dirPrev >= 0: hold and rising entries launch as before).
                attGap = critNum;
                flatChord = critNpV==critVal;
                attHot = flatChord&((dirPrev*critDl)<(3*attGap));
                attT = select2(attHot, critDl, max(1, min(critDl, Tsh)));
                T0 = max(1, select2(relTrig, attT, relT));
                p1t = select2(relTrig, critVal, v1);
                // landing chord / rideMax, ONE shared division: every
                // attack landing aims at the nearest strictly-deeper point
                // one scale out (the critical candidate's np pair; own
                // value as np value = land flat); the release side reads
                // this same quotient as rideMax = (v1 - gain)/(i1 + 1)
                // (the m0Tt path below). The two are consumed on opposite
                // sides of relTrig -- aim only in m1Tt/ckM1's attack
                // branches, rideMax only in m0Tt's release branch -- so
                // one division serves both, bit-exactly (dens >= 1 on
                // both branches, so the discarded value is always
                // finite). A schedule-aware release chord
                // (nhV - v1)/(nhD - i1) measured as a regression (see
                // the header); to fly one anyway, give rideMax its own
                // division and rebind aim over select2(relTrig, ...)
                // endpoints. nhD is dead without it, so its bank chain
                // compiles out.
                aim = (select2(relTrig, critNpV-p1t, v1-gain))/(select2(relTrig, max(1, critNpD-critDl), i1+1));
                // Fritsch-Carlson bound, carried x T: g3 = 3*(p1t - gain)
                // is a launch floor on attacks (g3 < 0), a launch cap on
                // releases (g3 > 0). All caps/floors below act on the
                // pre-multiplied tangents; scaling by Tt > 0 is monotone
                // and rounds monotonically, so min/max/select2 pick the
                // SAME branch a slope-form latch would, each branch value
                // that branch's slope*Tt bit-for-bit -- minus the
                // /Tt-then-*T rounding a slope form pays on the g3 arms.
                // the launch tangent is velocity-continuous in BOTH
                // directions, with the FC bound kept on the overshoot side
                // only -- the downside for attacks, the upside for
                // releases. dirPrev is a release leg's decaying slope when
                // an attack fires mid-rise (the attack-side pickup) and ~0
                // on launches from a hold. m0T <= g3 with m1T = 0 keeps a
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
                // within the documented class. max(aBT, rideT) keeps the ride
                // never slower than the plain chase (and discards a negative
                // rideMax the same way).
                rideK = 3.0/(float(nRel)*float(nRel));
                // aim IS rideMax = (v1 - gain)/(i1 + 1) on this branch:
                // the shared quotient above, release side. The x Tt scale
                // is applied OUTSIDE the min/max (bitwise-identical:
                // scaling by Tt > 0 is monotone and rounds monotonically),
                // which also keeps aim's quotient behind a min/max barrier
                // -- Faust's normal form reassociates a bare (n/d)*T into
                // (n*T)/d, splitting the shared division in two.
                dT = dirPrev*Tt;
                rideT = min(dirPrev+(nhV-v1)*rideK, aim)*Tt;
                // launch floor: never descend toward the deepest point (a
                // two-sided launch can swoop below the window min -- see
                // the header).
                aBT = min(g3, max(0, dT));
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
                // the Hermite basis through by T^3; the tangents enter
                // in their x T form directly -- max(g3, dirPrev*T) and
                // its m1 mirror, the SAME arms as the latch below but
                // scaled by the check length T0 (the latch lands at Tt,
                // which this check decides, so it cannot reuse the latch
                // values; all factors coerced to float first: d^3
                // overflows int32 at large windows). Disabled candidates
                // auto-pass (min(val, 2)
                // caps the ma.MAX sentinel and any GR is <= 1, so the
                // capped bound only ever relaxes a true one); deadlines
                // outside (0, T) auto-pass (dl >= T lands first and holds
                // at p1 <= val; dl <= 0 is the sample playing now).
                //
                // On failure the leg is SHORTENED, velocity kept -- the
                // shorten medicine, third verse. (Launching at the
                // argmin's own score instead
                // would be a one-sample velocity corner at every
                // engagement -- rising entries CHOPPED onto the near-flat
                // chord; rest entries snapped from 0 to the chord.)
                // Shortening is the safe direction: at fixed play time t,
                // dp/dT = 2*tau*(1-tau)*t*(m0T - g3)/T^2 >= 0 since
                // m0T >= g3 by construction in every regime, so
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
                // ~5 multiplies and two compares per candidate (Horner,
                // shared coefficients) plus ~3
                // more for the two trees, one division, one sqrt, one
                // ceil; no state.
                ckT = 1.0*T0;
                ckT2 = ckT*ckT;
                ckT3 = ckT2*ckT;
                ckM0 = max(g3, dirPrev*ckT);
                ckM1 = select2(relTrig, max(g3, min(0, aim)*ckT), 0);
                // the check cubic in Horner form. pl below ==
                //   gain*(2*df3-3*df2*ckT+ckT3) + ckM0*(df*(ckT-df)^2)
                //   + p1t*(df2*(3*ckT-2*df)) + ckM1*(df2*(df-ckT))
                // regrouped by powers of df. The coefficients depend only
                // on the latch, not the candidate, so they are computed
                // ONCE per sample and each candidate pays 3 multiplies for
                // the cubic (plus df2 for the zipper num and the shared
                // valC*ckT3 bound) instead of ~14. Re-association moves the
                // rounding at the pass boundary only: a flip there shortens
                // or relaxes T by rounding noise, inside the check's own
                // slack, and the deadline clamp + per-sample re-latch
                // backstop both directions as always.
                ckA = gain*ckT3;
                ckB = ckM0*ckT2;
                ckC = ckT*(g3-2*ckM0-ckM1);
                ckD = 2*(gain-p1t)+ckM0+ckM1;
                clearOne(val, dl, npv, npd) = num, den, dlP
                    with {
                        df = 1.0*dl;
                        df2 = df*df;
                        valC = min(val, 2.0);
                        pl = ckA+df*(ckB+df*(ckC+df*ckD));
                        pass = (dl<=0)|(df>=ckT)|(pl<=(valC*ckT3));
                        num = select2(pass, df2, 1e30);
                        den = select2(pass, max(1e-30, gain-valC), 1.0);
                        dlP = select2(pass, df, 1e30);
                    };
                // checkEvery gates which scales the check evaluates:
                // candidate i is live iff i % checkEvery == 0 (always
                // includes candidate 0, the full window). kp is a
                // compile-time constant, so checkEvery = 1 folds to the
                // ungated graph exactly (bit-identical, zero cost) and
                // skipped candidates' work is dead-code-eliminated.
                // Skipped scales fall to the deadline clamp +
                // per-sample re-latch backstops, like the dyadic
                // shadowing always has -- see `checkEvery` at the
                // wiring for the measured trade.
                chk(i) = clearOne:(select2(kp, 1e30, _), select2(kp, 1.0, _), select2(kp, 1e30, _))
                    with {
                        kp = (i%checkEvery)==0;
                    };
                checks = cands:(si.bus(4*nC), si.block(2)):par(i, nC, chk(i));
                clrPair = checks:par(i, nC, (_, _, !)):mpTree(nC);
                Tneg = ceil(sqrt(max(0.0, (gain-p1t)*(clrPair:(_, !))/(clrPair:(!, _)))));
                Tpos = checks:par(i, nC, (!, !, _)):minTree(nC);
                Tclr = select2(dirPrev>0, Tneg, Tpos);
                // Tpos doubles as the clear flag: every failed candidate
                // contributes dlP = df < ckT <= 1e30 and every passing one
                // the 1e30 sentinel, so "some check failed" == Tpos < 1e30,
                // exactly -- no separate pass AND-tree is needed.
                Tt = select2((relTrig==0)&(Tpos<1e30), T0, max(1, min(T0, Tclr)));
                m0Tt = select2(relTrig,
                    max(g3, dT),
                    select2(liftAhead, aBT, max(aBT, rideT)));
                m1Tt = select2(relTrig, max(g3, min(0, aim)*Tt), 0);

                TN = select2(trig, T, Tt);
                p0N = select2(trig, p0, gain);
                m0TN = select2(trig, m0T, m0Tt);
                p1N = select2(trig, p1, p1t);
                m1TN = select2(trig, m1T, m1Tt);
                // segments start at k = 1: first step on the trigger sample, so
                // per-sample re-triggers re-plan instead of stalling, and the
                // trigger sample keeps the previous velocity (p(1/T) ~= p0 + m0T/T)
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
                // the x T states save the two TN tangent multiplies here
                hermiteVal = h00*p0N+h10*m0TN+h01*p1N+h11*m1TN;

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
// `checkEvery`: clearance-check resolution knob. 1 checks every scale
// the bank resolves; the gate folds at compile time, so 1 compiles to
// the exact ungated graph at zero cost. 2 (the shipped constant)
// checks every other scale: measured ~10-16% faster overall on an x86
// test box at maxSR = 48000. Empirical quality result, not a theorem:
// on the
// calibrated torture workload (noise level 1, 480k samples, check
// binding at ~4k samples with path reshaping up to 0.33 vs no check),
// checkEvery = 2 was BIT-IDENTICAL to 1 -- every binding engagement
// was caught by the even scales. Material with odd-scale-only
// failures would see coarser zipper refusal there, bounded as always
// by the deadline clamp + per-sample re-latch backstops. The
// brickwall guarantee is structural (gain <= v1), does not depend on
// the check at any setting, and measured exactly 0 violation even
// with the check removed entirely.
checkEvery = 2;

lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR) = lookaheadAttackReleaseSmootherCk(nAtt, nRel, maxAtt, checkEvery, rawGR);

// fully parameterized variant, for callers that want the knob as an
// argument instead of the constant above
lookaheadAttackReleaseSmootherCk(nAtt, nRel, maxAtt, checkEvery, rawGR) = hermiteAttackReleaseFollower(nB+1, nRel, checkEvery, cands)
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
DJcompGroup(x) = vgroup("[0]DJ comp", x);
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
// follower: GR as it looks with the release applied upstream --
// descents stay steps (the lookahead's job), every rise is a smooth
// exponential, so attacks launch from a MOVING constraint. A/B it
// against the raw signals to see what shaping the release inside
// the smoother buys.
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

relHoldMs = SmootherGroup(hslider("[0]rel hold[unit:ms][scale:log]", 50, 0.1, maxRelHold*1000, 0.1));
attMs = SmootherGroup(hslider("[1]attack lookahead [unit:ms]", 25, 0, 50, 0.1));
nAtt = max(2, min(maxAtt, int(attMs*0.001*ma.SR)));
relMs = SmootherGroup(hslider("[2]release [unit:ms]", 50, 0, 500, 0.1));
nRel = max(1, int(relMs*0.001*ma.SR));

// Shape slider base: g = shapeBase^s, so |s| = 1 gives a
// shapeBase^2 : 1 endpoint-velocity skew. Must stay > 1.
shapeBase = 6;
// shapeBase = SmootherGroup(hslider("[2a]shape base", 8, 4, 16, 1));

gAtt = pow(shapeBase, attShapeSl);
// v1.15.0: the release slider drives the FRONT-LOAD Moebius warp --
// the ORIGINAL v1.9.1 mapping, restored. The warp keeps m0 = 0
// (h01'(0) = 0 at every g), so the release starts FLAT from the
// hold at any setting; g = 4^-s tightens the bottom knee and
// stretches the tail: sine at 0, pointy S at 1. This mapping was
// right all along -- it read as broken for five versions because
// the receding-deadline composite (v1.10.0..v1.13.0) re-aimed the
// leg at the rising reveal and back-loaded every non-block input;
// THE UNITY TARGET (v1.14.0) fixed that, so the warp now expresses
// on every input exactly as on blocks. relEase (the one-pole-style
// launch boost, v1.12.x) stays in the API for callers that want a
// non-flat exponential onset; the demo keeps the flat-start S
// contract and pins it to 0.
gRel = pow(shapeBase, 0-relShapeSl);
relEase = 0.0;

// ============================================================================
//  GUI
// ============================================================================

grMeter = DJcompGroup(hbargraph("[00]gain reduction[unit:dB]", -24, 0));
refMeter = DJcompGroup(hbargraph("[01]ref[unit:dB]", -24, 0));
dvMeter = DJcompGroup(hbargraph("[02]dv", 0, 1));

thres = DJcompGroup(hslider("[03]thres[unit:dB]", -1, -30, 0, 0.1));

attOvershoot = DJcompGroup(hslider("[04]attOvershoot[unit:dB]", 4.2, 0, 18, 0.1));
maxAttDJ = DJcompGroup(hslider("[05]maxAtt[unit:ms][scale:log]", 42, 1, 420, 1)*0.001);

startRelease = DJcompGroup(hslider("[06]startRelease[unit:ms][scale:log]", 13, 1, 3000, 1)*0.001);
endRelease = DJcompGroup(hslider("[07]endRelease[unit:ms][scale:log]", 69, 1, 3000, 1)*0.001);
transitionTime = DJcompGroup(hslider("[08]transitionTime[unit:ms][scale:log]", 420, 1, 3000, 1)*0.001);
transitionRange = DJcompGroup(hslider("[09]transitionRange[unit:dB]", -9, -18, -0.1, 0.1));

djLookMs = DJcompGroup(hslider("[10]lookahead[unit:ms]", 0, 0, maxDJLook*1000, 1));
underreact = DJcompGroup(hslider("[11]underreact[scale:log]", 42, 1, 10000, 0.1));

// ============================================================================
//  RELEASE HOLD (from lamb)
// ============================================================================
maxRelHold = 0.05;
maxRelHoldSamples = maxRelHold*maxSR;

// Clamped into the budget per the SR-budget note. Adds rel_hold_samples of
// latency on top of the input; at rel hold = 0 the stage is the identity
// (held == x, both delays are @0).
rel_hold_samples = int(relHoldMs*0.001*ma.SR:max(0):min(maxRelHoldSamples));

// - min(prevGain, x @ rel_hold_samples) keeps the output monotonically
//   non-rising: once we've gone down, we can't release.
// - slidingMin(rel_hold_samples+1, ...) is the future-min within the hold
//   window: we may go as low as that, but no lower is required.
// - max() of those two yields the held value.
// Falls always emerge from the x@rel_hold_samples arm, so held is presented
// exactly rel_hold_samples late.
releaseHold(x) = loop~_
    with {
        loop(prevGain) = max(min(prevGain, x@rel_hold_samples),
            x:ba.slidingMin(rel_hold_samples+1, 1+maxRelHoldSamples));
    };

// ============================================================================
//  DJ-comp
// ============================================================================

maxDJLook = 0.05;
maxDJLookSamples = int(maxDJLook*maxSR);
// tap count of the DJ demand ladder (v1.9.0): compile-time, from the
// bank's own int2nrOfBits over the maximum window
nBDJ = int(floor(log(maxDJLookSamples+1)/log(2))+1);

// Attack lookahead for the DJ computer (v1.5.0). Clamped into the
// budget per the SR-budget note, like rel_hold_samples. Adds
// dj_look_samples of latency on top of the input; at lookahead = 0
// every stage it feeds is the identity (slidingMin over 1 sample,
// @0), so v1.4.1 behavior is recovered bit-exactly.
dj_look_samples = int(djLookMs*0.001*ma.SR:max(0):min(maxDJLookSamples));

gain_computer(strength, thresh, knee, level) = select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
    0,
    ((level-thresh+(knee/2)):pow(2)/(2*max(ma.EPSILON, knee))),
    (level-thresh)):max(0)*-strength;

// TODO: put smoothing after channel-link in N-chan version
// Outputs (gain, holdGain): the DJ gain and the hold-processed raw GR
// it chases -- the compressor feeds BOTH into the smoother's overshoot
// cap (lookaheadAttackReleaseSmootherShapedOs).
compression_gain_mono_db_auto(strength, thresh, knee, level) = loop~(_, _):(_, !, _)
    with {
        loop(prevGain, prevRef) = gain, ref, playGain
            with {
                // rawGain = gain_computer(1, thresh-attOvershoot, knee, level)*strength;
                rawGain = gain_computer(1, thresh, knee, level)*strength;
                holdGain = rawGain:releaseHold;

                // --- DJ lookahead, spent on the ATTACK (v1.8.0; v1.9.0: the demand ladder) ---
                // The attack time itself is AUTOMATED by the distance
                // to the event. (lookGain, lookIdx) = the min over the
                // lookahead window [now .. now + dj_look] (play sample
                // included) and the play index of its FIRST occurrence
                // (idx 0 = the sample playing now), read exactly by
                // slidingMinIdx -- the bank's window machinery. ONE
                // glide chases the punch ceiling
                // lookTgt = min(0, lookGain + attOvershoot), and PLANS
                // AN EXACT LANDING: every sample the pole is re-derived
                // from the LIVE gap and the TRUE remaining distance, so
                // that riding it puts the state glideEps dB above the
                // ceiling exactly when the event's sample plays:
                //   tau = underreact^(lookIdx/dj_look)
                //         * lookIdx / (SR * ln(gap/glideEps))
                // The right factor alone IS the exact plan; the boost
                // in front is the deliberate UNDERREACTION --
                // underreact x too slow at the horizon, decaying to 1
                // (the exact plan) as the event arrives. Re-planning
                // makes it SELF-CORRECTING: whatever the early sloth
                // leaves undone re-enters the next sample's gap, and
                // the remaining schedule steepens by exactly that
                // much, so the landing stays exact for ANY underreact
                // -- U shapes WHEN the work happens, never whether.
                // U -> 1 is the eager end: a constant pole from entry
                // to landing, a straight line in dB. Large U piles the
                // work ever later -- the log-gap fraction closed by
                // remaining distance r is
                //   1 - exp(-int_r^L ds / (s * U^(s/L)))
                // which still -> 1 as r -> 0 for any U: the integrand
                // is ~1/s near the deadline, per-sample authority is
                // 1/r of the REMAINING log-gap, so the finish stays
                // continuous, late steps shrinking with the gap they
                // close. Deeper mins landing in the window, mins
                // stepping closer, ties to the SOONEST occurrence: all
                // re-plan the same way, immediately.
                // The settle through the punch band stays the
                // untouched v1.4.1 pole at maxAttDJ, so the punch
                // contract (at most attOvershoot dB above the playing
                // demand) is exactly preserved -- the glide hands the
                // settle a punch band grown by the glideEps hair. The
                // glide is NOT floored at maxAttDJ any more: the
                // schedule is the authority, and a plan left late
                // legitimately outruns the settle pole. Events
                // shallower than attOvershoot pin lookTgt to 0: inert,
                // the punch band is v1.4.1 verbatim.
                // The RELEASE is untouched: the glide is gated inert
                // unless its target is BELOW the state, so with
                // nothing deep in sight the pole releases toward
                // playGain exactly as v1.4.1 -- but a looming peak
                // binds mid-release and bends it smoothly down
                // instead of letting it pump out and attack straight
                // back in.
                // THE DEMAND LADDER (v1.9.0). Single-event focus --
                // pre-chasing only the window min -- had two failure
                // modes, both "the lookahead attack doesn't fire":
                // * STOLEN PLAN: a deeper sample landing later takes
                //   over (lookGain, lookIdx); the sooner, shallower
                //   demand is abandoned to the deadline cap, and with
                //   attOvershoot = 0 the cap is playGain itself -- a
                //   raw-speed cliff of the whole residual at the
                //   abandoned deadline. Measured 3+ dB single-sample
                //   yanks on plain two-step pairs 30 ms apart.
                // * PINNED SLOTH: while holdGain still makes new
                //   record lows at the horizon (any ramped or bursty
                //   attack), every record re-enters at lookIdx =
                //   dj_look, attDV pins at 1, and the plan crawls at
                //   underreact x too slow for the ENTIRE record-making
                //   phase; the already-revealed near portion of the
                //   descent replays through the cap instead of being
                //   flown.
                // Both are the same hole: the schedule authority was
                // derived from ONE (value, idx) pair, so the pending
                // demands the window had already revealed carried no
                // authority of their own. The fix samples the ceiling
                // schedule C(s) = min over play [0, s) at dyadic
                // horizons -- the prefix taps of slidingMinIdxBankDJ,
                // the bank's attack-tap alignment reused on the DJ
                // window -- and flies ONE glide step per rung: same
                // exact-landing pole, same gate, each rung's
                // underreaction from its EDGE DEADLINE -- the far
                // edge 2^j - 1 of its prefix window, not the min's
                // exact position, so the rung needs no timestamp
                // lane and its authority is control-rate. The
                // glide is the rungs' LOWER ENVELOPE (min): near
                // rungs commit for what is about to play while far
                // rungs keep the horizon sloth, so every revealed
                // demand is served at its scale and nothing is left
                // to the cap but the glideEps landing hair. An
                // isolated event renders BIT-IDENTICALLY to v1.8.0:
                // the full-window rung below keeps the true
                // (value, idx) plan verbatim, it is the deepest and
                // slowest rung, and on a lone event it IS the
                // envelope. Edge deadlines serve a demand up to one
                // scale late -- the same bound dyadic shadowing
                // already imposed with true per-tap indices -- and
                // the next tighter rung picks the demand up as it
                // crosses each edge, so the residual lands through a
                // hotter, still-smooth pole one class below the
                // removed cliffs; the downstream Hermite window
                // absorbs what is left. dj_look = 0 gates
                // every rung inert (0, above any real state):
                // v1.4.1 recovered, as before.
                // Cost: the pair cascade is unchanged; the taps add
                // nBDJ value-lane alignment delays (the Att bank's
                // own trick, half its lanes). The pole is GAP-AWARE,
                // so its final exp lives INSIDE the recursion --
                // live re-planning, like the Hermite side -- and the
                // loop proper pays one log + one exp + a mult chain
                // PER RUNG per sample, nBDJ + 1 rungs (14 + 1 at the
                // default budget, of which only the rungs at or
                // under the live window are ever engaged). Everything
                // else (edge divisions, underreact exps) is
                // CONTROL-RATE in tapRate. Measured on the full
                // chain, 48 kHz double: +8% process time over
                // v1.8.0, compile 2m20s vs 0m49s -- the enlarged
                // recursion outgrows faust's DEFAULT 120 s
                // self-timeout, so pass -t <big> or the compiler
                // dies silently at exactly two minutes. Rungs
                // shallower than the downstream Hermite window are
                // redundant insurance (nAtt absorbs sub-window
                // steps); drop the smallest few from the par() if
                // the exp budget pinches, though measured compile
                // time barely moves with rung count.
                lookBank = holdGain:slidingMinIdxBankDJ(dj_look_samples+1, maxDJLookSamples+1);
                lookGain = lookBank:ba.selector(0, nBDJ+2);
                lookIdx = lookBank:ba.selector(1, nBDJ+2);
                tapV(j) = lookBank:ba.selector(2+j, nBDJ+2);
                // Edge deadline of rung j: the far edge of its prefix
                // window, floored like lookIdx. No timestamp lane --
                // planning against the edge keeps the identical
                // one-scale-late worst case (the next tighter rung
                // picks the demand up as it crosses each edge) and
                // makes the rung's authority CONTROL-RATE.
                tapI(j) = max(1, min((1<<j)-1, dj_look_samples));
                playGain = holdGain@dj_look_samples;

                lookTgt = min(0.0, lookGain+attOvershoot);
                // Per-tap plans: target, horizon fraction and
                // authority of rung j -- target audio-rate off the
                // value lane, authority control-rate off the edge.
                // Inactive taps read
                // (ma.MAX, 1), so tapTgt pins to 0 and the strict gate
                // in the loop reads them inert.
                tapTgt(j) = min(0.0, tapV(j)+attOvershoot);
                tapDV(j) = (tapI(j)*(1.0/max(1, dj_look_samples))):max(0.0):min(1.0);
                tapRate(j) = (1.0/max(1.0, tapI(j)))*exp(0.0-tapDV(j)*log(underreact));
                // 0 = the event plays NOW, 1 = event at the
                // horizon. The reciprocal multiply (not /) hoists
                // THIS division to the control block; the plan's
                // own 1/lookIdx cannot -- it rides along in
                // lookRate below.
                attDV = (lookIdx*(1.0/max(1, dj_look_samples))):max(0.0):min(1.0);
                // Landing tolerance: the glide plans to sit glideEps
                // dB above the punch ceiling when the event plays;
                // the settle inherits a punch band grown by that
                // hair. It doubles as the hold band: gaps at or
                // under it pin the pole to 1 (see the loop).
                // Constant: 1/glideEps folds at compile time.
                glideEps = 0.1;
                // SmootherGroup(hslider("glideEps[unit:dB]", 0.1, ma.EPSILON, 12, 0.1));

                // The plan's per-sample authority: the fraction of
                // the REMAINING log-gap one pole step closes,
                //   lookRate = 1 / (lookIdx * underreact^attDV)
                // -- exact landing wants 1/lookIdx of it per sample,
                // the boost withholds by up to underreact at the
                // horizon. Audio-rate but FEEDBACK-FREE (only the
                // gap term needs yState), so it hoists out of the
                // recursion, division and all; ln(underreact) folds
                // to the control block.
                lookRate = (1.0/max(1.0, lookIdx))*exp(0.0-attDV*log(underreact));

                gain = playGain:onePoleSwitching(gainRel, maxAttDJ);

                onePoleSwitching(att, rel, x) = loop~_
                    with {
                        loop(yState) = min(min((1.0-coeff)*x+coeff*yState, glide), min(0, playGain+attOvershoot))
                            with {
                                coeff = ba.if(x>yState, ba.tau2pole(att), ba.tau2pole(rel));
                                // The exact-landing pole, re-planned from the
                                // LIVE gap every sample:
                                //   a = (glideEps/gap)^(1/lookIdx)
                                // slowed by underreact^attDV -- opened up,
                                // with everything the feedback does NOT need
                                // hoisted into lookRate above:
                                //   a = exp(-ln(lookRatio)*lookRate)
                                // lookRatio = max(gap/glideEps, 1): at or
                                // under the tolerance -- and whenever the
                                // glide is gated off, gap <= 0 -- ln = 0 and
                                // a = 1: a clean HOLD glideEps above the
                                // ceiling until the event plays and the
                                // settle walks the punch band. The same
                                // clamp is the NaN guard (select2 computes
                                // BOTH branches). lookIdx floored at 1 plans
                                // the last pre-play sample as a one-step
                                // landing -- by then the boost has decayed
                                // and the gap is ~glideEps, so that step is
                                // a hair, not a yank.
                                // glideAt(tgt, rate): ONE exact-landing
                                // step toward one (target, authority)
                                // pair; inert (0, above any real state)
                                // unless the target is below the state.
                                glStep(a, t) = a*yState+(1.0-a)*t;
                                glideAt(tgt, rate) = select2((dj_look_samples>0)&(tgt<yState), 0.0, glStep(pole, tgt))
                                    with {
                                        ratio = max((yState-tgt)*(1.0/glideEps), 1.0);
                                        pole = exp(0.0-log(ratio)*rate);
                                    };
                                // v1.9.0: the glide is the LOWER
                                // ENVELOPE of one such step per ladder
                                // rung -- the full window plus every
                                // dyadic prefix tap, each flying its
                                // own plan at its own scale's
                                // underreaction. Engaged rungs are
                                // <= 0, inert rungs read 0, so the min
                                // is the envelope and all-inert
                                // recovers the plain one-pole exactly
                                // as before.
                                glide = (glideAt(lookTgt, lookRate), par(j, nBDJ, glideAt(tapTgt(j), tapRate(j)))):ba.parallelMin(nBDJ+1);
                            };
                    };

                gainRel = interpolate_logarithmic(dv, endRelease, startRelease);

                attCurve(shape, dv) = select2(abs(shape)<2e-3,
                    pow(r, dv)*g-r*g// == (r^dv - r)/(1 - r), exact
                    ,
                    1-dv)// r -> 1 limit
                    with {
                        r = pow(1000, shape);
                        den = 1-r+select2(abs(1-r)<ma.EPSILON, 0, ma.EPSILON);
                        g = 1/den;
                    };
                refAttackTime = 0;
                singleprecisionMAX = 3.402823466e+38;

                ref = (prevGain-transitionRange):min(0)*strength:si.onePoleSwitching(refRel, refAttackTime);
                //:refMeter;
                refRel = it.interpolate_linear(dv,
                    transitionTime,
                    singleprecisionMAX/128);
                fastGR = (prevGain-prevRef);
                dv = (fastGR/transitionRange):max(0):min(1);
                //:dvMeter;
            };
    };

// v0*pow(v1/v0, dv) with the pow opened into exp(dv*log(v1/v0))
// (v1.16.2, perf): valid for v0, v1 > 0 (they are times here), and
// log(v1/v0) depends only on sliders, so Faust hoists it to the
// control block -- the audio path pays ONE exp instead of a full
// pow. ulp-level differences from the pow form only (libm pow is
// exp(y*log(x)) plus edge-case guards).
interpolate_logarithmic(dv, v0, v1) = v0*exp(dv*log(v1/v0));

// audio-rate dB -> linear (v1.16.2, perf): pow(10, x/20) opened into
// exp(x*(log(10)/20)) -- the constant folds at compile time, so the
// per-sample cost is ONE exp instead of a full pow. ulp-level
// differences from ba.db2linear only. Control-rate sites (osL) and
// the demo keep ba.db2linear.
db2linearFast(x) = exp(x*(log(10.0)*0.05));

compressor(l, r) = l@latency*gain, r@latency*gain, scopeGain, gain
    with {
        rawGR = compression_gain_mono_db_auto(1, thres, 0, max(abs(l), abs(r)):ba.linear2db):(_, !);
        playGain = compression_gain_mono_db_auto(1, thres, 0, max(abs(l), abs(r)):ba.linear2db):(!, _);
        gain = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR):grMeter:db2linearFast;
        // scopeGain = select2(SmootherGroup(checkbox("[99]rawGR")), playGain, rawGR):db2linearFast@(nAtt-1);
        scopeGain = compression_gain_mono_db_auto(1, thres, 0, max(abs(l), abs(r)):ba.linear2db):select2(SmootherGroup(checkbox("[99]playGain"))):db2linearFast@(nAtt-1);
        // scopeGain = rawGR:db2linearFast@(nAtt-1);

        // the DJ computer stays dB inside; the smoothing chain is
        // LINEAR from here (v1.13.0, THE LINEAR DOMAIN in the header)
        // preGainL = preBoth:(_, !):db2linearFast;
        // preHoldL = preBoth:(!, _):db2linearFast;
        // gainL = lookaheadAttackReleaseSmootherShapedOs(nAtt, nRel, gAtt, gRel, relEase, maxAtt, attOvershoot, preGainL, preHoldL);
        // gain = attach(gainL, (gainL:ba.linear2db:grMeter));
        // dj_look_samples: the DJ gain rawGR(t) is planned for the
        // input sample dj_look_samples further in the past, so play
        // alignment gains that term. The rawGR play tap stays at
        // @(nAtt-1): rawGR itself already carries the extra shift.
        latency = nAtt-1+rel_hold_samples+dj_look_samples;
    };

process = MainGroup(compressor);

demoGR = MainGroup(demo(testSignal))
    with {
        demo(rawGR) = grPlay, smoothed
            with {
                grPlay = de.delay(maxAtt-1+maxRelHoldSamples, nAtt-1+rel_hold_samples, rawGR):ba.db2linear;
                smoothed = lookaheadAttackReleaseSmootherShaped(nAtt, nRel, gAtt, gRel, relEase, maxAtt, (rawGR:releaseHold:ba.db2linear));

                // -                grPlay = de.delay(maxAtt-1, nAtt-1, rawGR);
                // -                smoothed = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR);
            };
    };
