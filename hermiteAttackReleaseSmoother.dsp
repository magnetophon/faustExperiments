declare name "hermiteAttackReleaseSmoother";
declare version "1.9.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
import("stdfaust.lib");

// TODO: DJ gain release gets a ceiling target that it releases towards
// DJ gain attack: DONE in v1.9.0 -- the adaptive interpolator is superseded
// by the overshoot cap in the smoother (lookaheadAttackReleaseSmootherShapedOs
// + attMode 2); the old law is kept as attMode 0/1 for A/B.

//========================================================================
// Attack + release lookahead smoother. ONE Hermite-leg follower shapes
// both directions of a gain-reduction signal: descents chase the
// critical constraint read from a dyadic candidate bank (lookahead
// attacks), rises are latched Hermite release legs toward the
// attack-window min. No upstream release stage is needed, and idle
// HOLDS the gain rather than tracking the playing sample. Both leg
// families take a per-direction SHAPE parameter -- a time warp on
// the leg clock, neutral at g = 1 bit-identically -- see THE SHAPE
// WARP below. The requested g is a per-leg CEILING, not a
// constant: entries too hot for it, and clearance engagements, fly
// the FULL duration at the largest feasible g (down to neutral)
// before any duration is given up -- see THE FEASIBLE SHAPE
// (v1.6.0) below.
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
// early rather than braking instantly. (Since v1.6.0 a
// back-loaded release relaxes its warp first -- see THE FEASIBLE
// SHAPE -- so this shorten fires only past the NEUTRAL cap.) The
// no-overshoot lemma
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
// medicine as the release re-latch, mirrored -- and like it,
// since v1.6.0 the leg's warp relaxes toward neutral FIRST (THE
// FEASIBLE SHAPE); only past the neutral floor does it SHORTEN,
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
// (handoff) plus ~11 multiplies and two compares per candidate
// per sample (check; ~5 unwarped), no divisions, no state, no
// delay lines.
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
// THE SHAPE WARP (v1.5.0). Both leg families take a per-direction
// shape parameter by warping the CLOCK, not the curve: the basis
// is evaluated at u = w(k/T) with the Moebius map
//   w(t) = t/(t + g*(1-t)),  w(0) = 0, w(1) = 1, monotone,
//   w'(0) = 1/g,  w'(1) = g,
// one g per direction (gAtt, gRel > 0, control rate), latched per
// leg like T. g < 1 FRONT-loads a leg -- the speed lives near the
// launch: the "exponential" release feel, fast initial recovery
// into a long decelerating tail -- and g > 1 BACK-loads it: slow
// start, hot landing, the late-diving attack. g = 1 is the
// unshaped smoother BIT-IDENTICALLY, not merely equivalently:
// x*1.0 is exact, and k + g*(T - k) at g = 1 is integer
// arithmetic on integer-valued doubles, so u reproduces k/T to
// the bit, every latch arm's *1.0 fold vanishes exactly, and no
// compiler constant-folding is needed for the guarantee.
//
// Why a time warp survives the safety algebra where a basis
// change would not: a monotone reparameterization preserves the
// flight's VALUE SET exactly -- { H(w(tau)) } = { H(u) } -- so
// every lemma about the RANGE of a flight transfers verbatim: the
// no-overshoot bound above (m1 = 0 under the FC cap), its attack
// mirror, h11 <= 0 keeping the aim path <= v1, the launch floor
// never dipping below the window min, and hence the brickwall
// induction. Only statements about the value at a specific TIME
// change -- and those are exactly the ones the clearance check
// re-evaluates, against the warped plan. Durations are untouched:
// T stays in samples, a block release is still ONE leg of exactly
// nRel (now a skewed S), attacks still land on their deadlines,
// and u reaches exactly 1 at k = T (k + g*(T - k) = T + g*0: the
// integer-T landing argument is unchanged, so landed legs still
// close their triggers).
//
// C1 across joints survives through two control-rate constants:
// a latched leg's physical launch velocity is
// (m0T/T) * w'(0) = m0T/(g*T) and its landing velocity
// m1T * g / T, so every latch arm that carries a PHYSICAL slope
// is folded -- launches by * g (dT, rideT, ckM0's velocity arm,
// the capped test's nRel*g; shDir by the SHORTEN's flight g,
// min(g, 1) since v1.6.0, the ride's kept raw), landings by / g
// (aim's m1
// arm, ckM1) -- while the FC bounds themselves stay in u-space,
// untouched: min/max against g3 exactly as before. Scaling by a
// positive control-rate constant commutes bitwise with
// min/max/select2, so the x T latch arguments carry unchanged:
// capped launches still store g3 itself, and both shortens still
// put m0*T exactly ON the bound. Under per-sample re-latch the
// release cap doubles as before into the approach governor, now
// velocity <= 3*(v1 - gain)/(g*nRel): a front-loaded release
// legitimately recovers faster from the first sample. (The
// shapedSmoother's AUC compensation is the analogous knob if
// shape is wanted loudness-neutral -- scale nRel by the family's
// area ratio; not shipped here.) The ride's drive constant picks
// up the same launch-law factor (rideK * 1/gRel); its safety
// never lived in the drive value -- rideMax plus the per-sample
// induction own it, and the first emitted step of a latched leg
// is still the physical velocity (p(w(1/T)) ~= p0 + m0T/(g*T)),
// so the induction reads exactly as before.
//
// THE FEASIBLE SHAPE (v1.6.0). A back-loaded leg and a hot entry
// are incompatible over a long leg: the u-space launch tangent
// costs dirPrev*g*T, so at g > 1 the FC bound tolerates only 1/g
// of the physical entry velocity a neutral leg carries -- the
// hot-entry shortens fired g times more easily and cut g times
// deeper, and the zipper bound carried a further global
// zClr = 1/g. At high shape the safety machinery thus turned the
// requested LATE dive into an EARLY landing: legs slammed to the
// bottom in a fraction of the deadline and sat flat -- less
// smooth than the neutral smoother, the opposite of the knob's
// intent. v1.6.0 degrades the SHAPE per leg instead of the
// DURATION: the requested g is a ceiling, and each hot latch
// flies the largest feasible clock,
//   gEff = clamp(3*gap/(dirPrev*T), 1, gReq)   (T the full
//   deadline: critDl for attacks, nRel for releases),
// full-length whenever gEff >= 1 -- at gEff the FC bound sits
// exactly ON the entry velocity, so m0T lands exactly ON g3, and
// the raw-g fold arms sit past the bound BY THE HOT GATE, so
// every max()/min() saturates to g3 with no adapted-g arithmetic
// at all. Only entries too hot even for the NEUTRAL clock
// shorten, with the unwarped formula -- bit-identical to the
// g = 1 smoother's own medicine. gEff never needs a division of
// its own: it rides as the latched pair (Tq, T) with
// Tq = g3/dirPrev, the same quotient the shorten already pays,
// absorbed by the evaluator's and the check's own divisions (the
// ninth loop state is the pair's denominator). Front-loaded
// requests (g <= 1) are untouched: the clamp is the identity
// there, bit-for-bit. On clearance engagement of a flying g > 1
// the leg relaxes to NEUTRAL outright (pair (1, 1)): the zipper
// pull-back needs no wInv scale at g = 1 (zClr is GONE -- its
// 1/g was the single biggest over-shorten at high shape), the
// failed set of the taller plan covers the neutral flight
// (H(w_g(t)) >= H(w_1(t)) pointwise under FC monotonicity), and
// the shorten-monotonicity lemma is exact again on the flight
// actually flown. The ride keeps the raw g (its safety lives in
// rideMax + the per-sample induction, and a mid-ride relax would
// mis-scale the stored boosted launch), so relAdapt gates on
// liftAhead == 0.
//
// The clearance check keeps its exact play-time meaning by
// evaluating the WARPED plan: at deadline df the leg clock reads
// u = df/DD with DD = df + g*(ckT - df) -- linear in df, positive
// on the live range (0, ckT) -- and clearing DD^3 keeps the test
// division-free: the same cubic, contracted homogeneously in
// (df, DD), against valC*DD^3. Cost rises from ~5 to ~11
// multiplies per candidate; checkEvery halves it as always. The
// zipper-class closed form pulls back through the warp: the
// sufficient condition u >= sqrt(s) becomes
// tau >= wInv(sqrt(s)), wInv(x) = x*g/(1 - x*(1-g)). For g <= 1
// no scale is needed (wInv(x) <= x there); for g > 1 the engaged
// leg relaxes to NEUTRAL (THE FEASIBLE SHAPE), where wInv is the
// identity -- the unwarped pull-back is exact for the flight
// actually flown, and the old global zClr = min(1, 1/gAtt)
// factor (an up-to-g over-shorten) is gone.
//
// The accepted class: SHORTEN-MONOTONICITY. "A shorter leg only
// ever lowers the path" -- the lemma behind "passing candidates
// stay clear" in both Tclr regimes -- generalizes under the warp
// (velocity-kept, m1 = 0, fixed play time t) to
//   T*dp/dT = (1-u) * ( m0T*[u*(1-u) - tau*w'*(1-3u)]
//                       - 6*(p1-p0)*u*tau*w' )
// with the kernel u - tau*w'(tau) = u*tau*(1-g)/(tau + g*(1-tau))
// -- sign(1-g), cleanly. At g = 1 this collapses to the shipped
// 2u^2(1-u)*(m0T - g3) >= 0. Since v1.6.0 every clearance-
// shortened flight whose g exceeded 1 flies NEUTRAL (THE
// FEASIBLE SHAPE): the zipper bound is then self-contained (the
// m0 <= 0 majorant and its sqrt(s) condition are u-space facts,
// pulled back at the flight's own clock with no wInv scale), and
// the hot-RISING small-u residue of the warped shorten is gone
// with it. The relax opens one small class of its own: a PASSING
// candidate was cleared against the g-warped plan, and the
// neutral flight launches shallower than that plan by at most
// |dirPrev|*(g - 1)*Tclr*max(h10) in u-space -- a near-rest
// quantity (the zipper class has dirPrev ~ 0) that can poke
// above a barely-passing candidate. What remains besides it is
// the g < 1 FC-floor descending corner (a front-loaded request,
// untouched by the relax). Every residue shrinks with |g - 1|,
// sits on the standing backstops (deadline clamp + per-sample
// re-latch), and the brickwall never depended on the check at any
// setting (measured exactly 0 violation with the check removed --
// see checkEvery at the wiring).
//
// Cost: the audio hot path is dominated by the argmin tree and
// the clearance check; per-candidate scoring is DIVISION-FREE
// (slopes ride the tree as cross-multiplied (num, den) pairs,
// dens >= 1). The landing chord and the ride cap share ONE
// division (the numerator/denominator pairs are selected first,
// branch-exclusive by relTrig); the shortened legs share ONE
// division + ceil (Tshort and attTs, same exclusivity; its
// un-ceiled quotient Tq doubles as the feasible-shape pair's
// numerator, so gEff costs NO division of its own); Tclr adds
// one division + sqrt + ceil; with the warped clock u -- whose
// division REPLACES tau's, the g reciprocals being control rate
// -- that is still FOUR audio-rate divisions total. The tangent
// states are carried pre-multiplied
// by the leg length (m0T, m1T), keeping the per-latch delta
// division off the feedback critical path and two Hermite
// multiplies out of the evaluator. The clearance check adds ~12
// multiplies and two compares per candidate per sample (the cubic
// contracted homogeneously in (dfs, DDs) -- the latched pair's
// denominator folds in as one extra multiply -- Horner form,
// per-sample shared coefficients; ~5 unwarped) plus its two
// trees -- the all-pass AND tree is folded into the Tpos min tree
// (all-pass reads back as exactly 1e30). No
// state beyond the 9-wide loop (the latched warp rides as a
// (num, den) pair in the last two), no
// delay lines beyond the bank's.
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
// * The hump class (shrunk in v1.8.0): an attack launched off a
//   rising tangent used to poke above a flat-playing constraint
//   until the deadline-0 candidate clamped it -- a velocity chop.
//   The rising-entry feasible shape (see the header) now caps the
//   launch crest against the sample playing at the latch, C1
//   kept, so the flat-ceiling case is gone. What remains: a
//   ceiling that FALLS between candidate deadlines mid-flight can
//   still be poked briefly before the check / re-latch / deadline
//   clamp catch it. Escape hatch if out <= grPlay must be
//   bit-exact: min the output with the delayed raw GR, at the
//   price of a C1 corner at the touch point.
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
//
// THE RISING-ENTRY FEASIBLE SHAPE (v1.8.0). The hump class had a
// worst case the clearance check is structurally blind to: on a
// FLAT playing stretch every dyadic candidate's min first occurs
// at the sample playing now, so every candidate carries deadline
// 0 and hits the dl <= 0 auto-pass -- the check has nothing to
// test a rising launch against, and a back-loaded attack picked
// up hot off an interrupted release leg drifts above the playing
// level until the deadline-0 candidate wins the argmin and clamps
// in T = 1: a velocity chop, the kink. The fix is the feasible-
// shape medicine, third direction: bound the CREST at the latch.
// For a leg with launch product a = m0T >= 0, drop
// D = p0 - p1 > 0 and landing m1T <= 0, the crest height above
// the launch point is, exactly at m1T = 0 (an upper bound for
// m1T < 0, h11 <= 0),
//   E(a) = a^2 (4a + 9D) / (27 (a + 2D)^2)
// -- a u-space fact, so by the value-set lemma ONE condition
// covers every warp g. With (4a + 9D) <= 4.5 (a + 2D) the
// sufficient condition E(a) <= H inverts in closed form:
//   aMax = 3H + sqrt(H (9H + 12D)),
// H = max(0, val0 - gain) the headroom against the sample playing
// NOW (tap 0's value: exactly the constraint the check cannot
// see), overestimating E by <= 12.5% -- the safe direction. The
// fold is two min()s: m0Tt's attack arm and ckM0 become
// max(g3, min(., aMax)) -- bit-exact no-ops for descending and
// rest entries (the arm is <= 0 <= aMax there) and for every
// rising entry whose requested crest already fits. When the gate
// binds (dirPrev*gAtt*critDl > aMax, the raw-g arm past the bound
// BY THE GATE, so the min saturates to aMax with no adapted
// arithmetic), the leg flies the full deadline at the feasible
// clock as the latched pair (aMax, dirPrev*critDl) -- gEff =
// aMax/(dirPrev*critDl), never divided, the evaluator's ratio is
// scale-free -- so the physical launch velocity is exactly
// dirPrev: C1 across the joint, the crest lands under the playing
// level, and the deadline-0 clamp goes quiet. Floored at the
// neutral-or-requested clock (min(gAtt, 1), sharing gShA): a
// front-load is never imposed on a back-load request -- the
// v1.5.0 early-bottom disease -- so past the floor the min caps
// m0T at aMax with a latch corner of dirPrev - aMax/(gFloor*T),
// orders below the clamp step it replaces and only reachable at
// near-zero headroom. Residuals, all backstopped as before: a
// ceiling that FALLS between candidate deadlines mid-flight
// (existing check + per-sample re-latch + deadline clamp own it);
// an engaged (clearance-shortened) attRise leg relaxes to
// NEUTRAL outright at ANY gEff -- its pair denominator carries
// the full deadline while the engaged flight flies Tclr, so
// keeping the pair would launch hotter than dirPrev -- and the
// aMax cap rides the min through the relax, so the crest bound
// survives engagement. The gate self-guards its degenerate corners:
// critDl = 0 and vanishing dirPrev both read the gate false
// (aMax >= 0), so the pair's denominator never goes 0 on a taken
// branch. Cost: one sqrt and a few multiplies on the latch path,
// per sample, shared; no division, no state. At gAtt = 1 the
// shaped and unshaped entry points still agree bit-identically
// (both carry the cap); v1.8.0 does change the unshaped
// smoother's own rising launches -- their humps are now capped
// too, which is the point.
//
// THE GLIDE GOVERNOR (v1.9.1). The overshoot wrapper's slow arm
// feeds the smoother a diet the step-shaped world never served: a
// monotone SMOOTH descent (a one-pole settling into the band).
// There the window argmin is the NEWEST sample every sample --
// critDl pinned at nAtt - 1, RECEDING -- so the creep gate reads
// steeper every sample, the leg re-latches per sample, and only
// FIRST steps of full-length legs ever play. Every attack latch
// is velocity-preserving by design (the C1 contract), and the
// first step of a velocity-kept T-length cubic is dirPrev again,
// so the follower degenerates to an integrator:
//   v <- v*(1 - 2/(gT)) + 3*gap/(gT)^2
// -- velocity time constant gT/2, the same order as the
// schedule's own pole. Symptom, both phases: an attack fired
// mid-rise COASTS upward (the crest arc's rising first step,
// re-latched forever, never turns) while the schedule descends
// underneath, then the repayment integrates into a too-steep,
// too-STRAIGHT plunge that crosses the schedule's convex curve
// and lands early. Steps never showed it: their deadlines
// APPROACH, the creep gate flies legs whole, the S-curves play
// out.
//
// The medicine is the release ride's, mirrored: pull the entry
// velocity toward the winner's own score at every latch,
//   dirPrevP = dirPrev + (sReq - dirPrev)*gvK,
//   sReq = critNum/max(1, critDl),  gvK = min(1, 24/critDl).
// The fixed point is velocity == required average slope, i.e.
// the plan ON schedule: quasi-steady on a glide, the gain rides
// the schedule delayed by the window with residual lag
// 2/(gAtt*gvK) = critDl/(12*gAtt) samples, and gvK scaling with
// the deadline keeps that a fixed FRACTION of the horizon at
// every window size. Gated on dirPrev != 0.0 -- an exact test,
// holds emit bit-zero steps -- so REST entries are bit-identical
// to v1.9.0. Consumed by the ATTACK machinery only (shDir's
// attack arm, attHot/attAdapt, dpT/attRise, the launch fold dTA,
// ckM0, and the Tclr class split), so the clearance check clears
// the SAME plan the latch flies, and every release branch keeps
// the raw dirPrev, bit-identical. Flown-whole legs feel it once,
// at their single latch: a mid-rise step entry blends 24/critDl
// (~1%) of the score into its launch -- inside the existing
// latch-corner budget, measured <= 0.09 dB on a mid-rise step
// battery. The clamp min(1, .) keeps small-deadline latches
// stable (deadbeat at worst); the pull is a per-sample
// ACCELERATION bounded by gvK*|sReq - dirPrev|, not a velocity
// corner. Cost: ONE division (the score -- off the min-tree's
// critical path, it consumes the tree's output) plus a multiply
// and a select; no state. The 24 is empirical like checkEvery,
// measured on the glide battery: crest coast gone, plunge slope
// from 2.3x down to 1.3x the local schedule slope, brickwall
// exactly 0 throughout, noise diet within 0.07 dB of v1.9.0.
// Residual, documented not hidden: the governor rides the
// CHORD-AVERAGE to the pin, so at a schedule CORNER (a flat
// ceiling, then the descent enters the window) it leads the turn
// by a few dB where the maximal path would hold the ceiling to
// the last sample; closing that needs a schedule-aware plan
// (candidate-shape-FITTING, not just candidate-clearing) --
// future work.
//
// THE AUC COMPENSATION (v1.7.0). Shaping a leg changes its area
// under the curve (AUC), hence its perceived loudness: a
// back-loaded release (g > 1) spends longer near full reduction
// than a neutral one of the same nRel, so it sounds louder in
// reduction; a front-loaded one (g < 1) sounds quieter. OPTIONAL,
// off by default, one checkbox each (attAucComp / relAucComp), a
// DEMO/GUI feature -- the CORE is untouched, taking raw g and raw
// durations exactly as before. When a box is on, the leg's
// DURATION is rescaled by a shape-derived area factor so the
// loudness tracks the neutral leg's; the single leg stays a pure
// scaled curve (the fold lands on the demo nAtt/nRel, before the
// int()/clamp, so lookahead sizing and the leg clock use the same
// compensated count -- grPlay's alignment delay reads that same
// nAtt and tracks for free).
//
// The factor is derived from THIS smoother's OWN warped-Hermite
// leg, NOT reused from shapedSmoother's auc_poly.lib: the shape
// here lives in the Moebius time-warp w(t) = t/(t + g*(1-t)) on a
// cubic Hermite, a different curve family from shapedSmoother's
// cheapCurveBase, so its polynomial would compensate the wrong
// area. The from-rest leg is value(tau) = h01(w(tau)),
// h01(u) = 3u^2 - 2u^3, whose area over [0,1] has the closed form
//   I(g) = (2g^3 - 6 g^2 ln g + 3 g^2 - 6 g + 1) / (g-1)^4,
// computed inline (no bake step, no .lib) -- verified to machine
// precision against a 4e5-point midpoint integral at
// g in {1/4, 1/2, 2, 4}. It is 0/0 -> 1/2 at g = 1 and loses
// float conditioning as |g-1| -> 0 (the (g-1)^4 amplifies), so
// the neutral band |g-1| < 0.1 uses the exact Taylor series about
// g = 1 (accurate < 1e-10 there, matching the closed form at the
// switch to ~1e-10: smooth, no step). The closed arm's g is
// forced out of the band before its denominator, since select2
// evaluates both arms and 0/0 would poison the blend.
//
// The factor NORMALIZES to <= 1 (sharpest shape maps to 1):
// aucLevelMult(g) = I(g_sharp)/I(g), g_sharp = 4 (the minimum
// area the slider reaches), so it only ever SHORTENS -- the
// maxAtt budget can only get colder, no allocation grows, and the
// reported latency (nAtt - 1) varies with shape exactly as in
// shapedSmoother, intended. Per-direction and independent: gAtt
// and gRel can warp opposite ways at once, each gets its own
// factor.
//
// The two directions accumulate reduction differently, so they
// feed the factor differently. An ATTACK DIVES into reduction:
// the reduction-area it racks up over the leg IS the leg area
// I(gAtt), so the attack feeds gAtt directly. A RELEASE RECOVERS
// out of reduction: the leg rises 0 -> 1 toward the (quiet) target,
// so its reduction-area is 1 - I(gRel), and by the exact identity
// 1 - I(g) = I(1/g) the release simply feeds 1/gRel into the SAME
// aucLevelMult -- reflecting front-load <-> back-load. This holds
// duration * (time spent deep in reduction) constant, the correct
// way round: a back-loaded release that lingers deep is shortened,
// a fast front-loaded recovery is not. Feeding gRel raw (the first
// cut) compensated the recovery-area instead and ran REVERSED --
// shortening the quiet front-loaded leg hardest.
//
// The g fed in is the REQUESTED (ceiling) g from the slider map, a
// slider-rate GUI concern -- NOT the per-leg feasible gEff (THE
// FEASIBLE SHAPE), which is a hot-entry runtime property and must
// not feed back into duration sizing. Folded via the branchless
// switched blend, exactly as shapedSmoother:
// aucLevelMultSwitched(on, g)
//   = 1 + on*(clamp01(aucLevelMult(g)) - 1); on in {0,1} so OFF
// is bit-exact 1.0 and nAtt/nRel revert bit-identically -- the
// g = 1 neutral guarantee the whole file rests on is undisturbed.
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
slidingMinIdxBankAtt(nAtt, maxAtt, x) = (v1, i1, npFullV, npFullD), par(i, nB, (outV(i), outD(i), npTV(i), npTD(i))), (nhV, nhD)
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
        // last pow2(i) raw input samples
        casc = (x, ba.time):sequentialOperatorParOut(nB-1);
        cV(i) = casc:ba.selector(2*i, 2*nB);
        cT(i) = casc:ba.selector(2*i+1, 2*nB);

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
        wV = par(i, nB, cV(i)):ba.selectn(nB, jW);
        wT = par(i, nB, cT(i)):ba.selectn(nB, jW);
        // dW never exceeds max(2^(nB-2)-1, maxAtt-2^(nB-1)): for
        // jW <= nB-2, dW < 2^jW <= 2^(nB-2); for jW = nB-1,
        // dW <= maxAtt-2^(nB-1). Compile-time int, so the two block
        // delay lines size to it instead of to 2^(nB-1) -- half the
        // added memory at the default maxAtt.
        dMax = max(0, max(pow2(max(0, nB-2))-1, maxAtt-pow2(nB-1)));
        fullA = (wV, wT, (wV:de.delay(dMax, dW)), (wT:de.delay(dMax, dW))):minIdxOp;
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

        // shared helpers (op bound to minIdxOp):
        sequentialOperatorParOut(N) = seq(i, N, operator(i));
        operator(i) = si.bus(2*i), (si.bus(2)<:(si.bus(2), ((si.bus(2), par(j, 2, _@pow2(i))):minIdxOp)));
        maxNrBits(m) = int2nrOfBits(m);
        pow2(i) = 1<<i;
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
// hermiteAttackReleaseFollower(nC, nRel, gAtt, gRel, checkEvery, cands) : _
// ```
//
// Where:
//
// * `nC`: number of candidates (compile-time int)
// * `nRel`: release leg length in samples (>= 1, may vary at control
//   rate); 1 = instant rises to v1
// * `gAtt`, `gRel`: per-direction shape (> 0, may vary at control
//   rate; latched per leg like T). 1 = the unshaped smoother,
//   BIT-identical; < 1 front-loads a leg, > 1 back-loads it -- see
//   THE SHAPE WARP in the header. A g > 1 is a per-leg CEILING:
//   hot entries and clearance engagements fly the full duration
//   at the largest feasible g down to neutral before any duration
//   is given up (THE FEASIBLE SHAPE). Floored at ma.EPSILON.
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
hermiteAttackReleaseFollower(nC, nRel, gAtt, gRel, checkEvery, cands) = (loop~si.bus(9)):(_, si.block(8))
    with {
        // release reads: candidate 0's value/deadline = the attack-window
        // min and the pin's play index; the tail = the next-higher pair
        v1 = cands:ba.selector(0, 4*nC+2);
        i1 = cands:ba.selector(1, 4*nC+2);
        // tap 0's value = min over the next 1 sample = the sample
        // playing NOW: the ceiling the rising-entry feasible shape
        // caps the launch crest against (see the header). Always
        // active (2^0 <= nAtt), so never the ma.MAX sentinel.
        val0 = cands:ba.selector(4, 4*nC+2);
        nhV = cands:ba.selector(4*nC, 4*nC+2);
        nhD = cands:ba.selector(4*nC+1, 4*nC+2);

        // the warp constants, control rate: floors keep the contract
        // g > 0 (max(eps, 1) == 1 exactly, so the neutral path is
        // untouched), the reciprocals stay out of the audio loop.
        gA = max(ma.EPSILON, gAtt);
        gR = max(ma.EPSILON, gRel);
        invGA = 1.0/gA;
        invGR = 1.0/gR;
        nRelG = nRel*gR;
        // the shorten/feasible-shape divisor factors: min(g, 1) is
        // the flight g of a past-neutral shorten (the requested g
        // for front-loads, neutral for back-loads), so Tq/Tsh come
        // out unwarped exactly where the relax applies and exactly
        // as before where it does not.
        gShA = min(gA, 1.0);
        gShR = min(gR, 1.0);
        nRelF = 1.0*nRel;

        // state: gain, p0, m0T, p1, m1T, k, T, gNL, gDL
        // (previous-sample values
        // inside loop). The tangent states are carried PRE-MULTIPLIED by
        // the leg length (m0T = m0*T, m1T = m1*T): the Hermite evaluator
        // only ever reads the products (h10*T*m0, h11*T*m1), so latching
        // the product keeps the per-latch delta division off the
        // feedback loop's critical path and two multiplies per sample
        // out of the evaluator. min/max/select2 all commute bitwise with
        // scaling by the (positive) leg length, so each latch branch is
        // the plain-slope branch's value*T bit-for-bit, and a capped
        // launch stores its bound exactly rather than a divide-then-
        // remultiply ulp off it -- see g3 below. With the warp the
        // products are u-space (m0T = g * physical launch velocity * T,
        // m1T = physical landing velocity * T / g); the leg's latched
        // warp g rides as the PAIR (gNL, gDL) -- g = gNL/gDL, never
        // divided: the evaluator's and the check's own divisions
        // absorb the denominator (THE FEASIBLE SHAPE), and plain
        // latches store (g, 1.0) so the neutral and front-loaded
        // paths stay bit-identical.
        loop(gain, p0, m0T, p1, m1T, k, T, gNL, gDL) = gainN, p0N, m0TN, p1N, m1TN, kN, TN, gNN, gDN
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

                // ---- the glide governor (v1.9.1, see the header) ----
                // the winner's score = the required average slope,
                // divided ONCE here -- the only new division, off the
                // min-tree's critical path (it consumes the tree's
                // output). The entry velocity is pulled toward it at a
                // deadline-tied rate; min(1, .) keeps small-deadline
                // latches stable, deadbeat at worst. dirPrev != 0.0 is
                // an exact test (holds emit bit-zero steps), so rest
                // entries stay bit-identical, and only the ATTACK
                // machinery consumes dirPrevP -- every release branch
                // keeps the raw dirPrev, bit-identical.
                sReq = critNum/max(1, critDl);
                gvK = min(1.0, 24.0/max(1.0, dlF));
                dirPrevP = select2(dirPrev!=0.0, dirPrev, dirPrev+(sReq-dirPrev)*gvK);

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

                // the NEW leg's warp, three layers:
                // * gCk/invGCk -- the RAW per-direction g, branch-shared
                //   by relTrig. These drive the physical-slope FOLDS
                //   (dT, rideT, ckM0/ckM1, TtG/TtIG): on the adapt
                //   branches the raw-g arm always sits past the FC
                //   bound (that is what the hot gate MEANS), so every
                //   max()/min() saturates to g3 exactly -- the folds
                //   never need the adapted g. At g = 1 both read
                //   exactly 1.0 and every fold is the identity.
                // * (gN1, gD1) -- the pre-engage flight g as a pair
                //   (g = num/den, never divided): the requested g on
                //   cool branches, (Tq, T) on adapt (min-clamped to the
                //   requested g against eps-distorted Tq on vanishing
                //   gaps), min(g, 1) on a past-neutral shorten, the raw
                //   g on the ride's. The check's DD and the evaluator's
                //   clock absorb the denominator.
                // * (gNt, gDt) -- post-engage: a clearance engagement
                //   of a flying g > 1 relaxes the leg to NEUTRAL (1, 1)
                //   -- THE FEASIBLE SHAPE in the header -- and gF/invGF
                //   swing the folds to 1.0 with it, so the engaged
                //   leg's velocity-kept launch is honest at its actual
                //   clock. relaxCk reads the pair's own compare
                //   (gN1 > gD1 <=> g > 1, cross-multiplication-free
                //   since gD1 > 0).
                gCk = select2(relTrig, gA, gR);
                invGCk = select2(relTrig, invGA, invGR);
                // attRise overrides the descending-entry branches
                // (mutually exclusive with attHot by the sign of
                // dirPrev): the leg flies the full deadline at
                // gEff = aMax/dpT as the pair (aMax, dpT) -- launch
                // velocity exactly dirPrev, C1 -- floored at the
                // neutral-or-requested clock (gShA*dpT: never
                // front-load a back-load request; past the floor
                // the m0Tt min caps at aMax, the accepted latch
                // corner). The evaluator's ratio is scale-free, so
                // the pair needs no normalization.
                gAttN = select2(attRise, select2(attHot, gA, select2(attAdapt, gShA, min(Tq, gA*dlF))), max(aMax, gShA*dpT));
                gAttD = select2(attRise, select2(attAdapt, 1.0, dlF), dpT);
                gRelN = select2(relAdapt, select2(capped&(liftAhead==0), gR, gShR), min(Tq, gR*nRelF));
                gRelD = select2(relAdapt, 1.0, nRelF);
                gN1 = select2(relTrig, gAttN, gRelN);
                gD1 = select2(relTrig, gAttD, gRelD);
                // an engaged attRise leg relaxes to neutral even at
                // gEff <= 1: its pair denominator carries the FULL
                // deadline (dpT = dirPrev*critDl) while the engaged
                // flight flies Tclr < critDl, so keeping the pair
                // would launch hotter than dirPrev by up to gAtt --
                // at neutral the min(., aMax) keeps the crest bound
                // and the launch velocity lands at dirPrev (or
                // capped below it, the safe direction).
                relaxCk = engaged&((gN1>gD1)|attRise);
                gNt = select2(relaxCk, gN1, 1.0);
                gDt = select2(relaxCk, gD1, 1.0);
                gF = select2(relaxCk, gCk, 1.0);
                invGF = select2(relaxCk, invGCk, 1.0);

                // ---- new-segment values (only used when trig == 1) ----
                // momentum-preserving re-latch, feasible-shape first: a
                // release re-latch arriving with dirPrev above the
                // fresh-leg FC cap 3*(v1 - gain)/(gRel*nRel) would clamp
                // the launch onto that cap (m0 = min(3*delta, dirPrev))
                // -- a one-sample velocity corner whenever a leg re-plans
                // mid-flight (term 1 late in a leg; term 2 the sample a
                // lift blip appears). Instead the WARP relaxes first
                // (relAdapt): while the entry fits the NEUTRAL cap
                // (dirPrev*nRel <= 3*gap), fly the whole nRel at the
                // feasible g = 3*gap/(dirPrev*nRel) in [1, gRel) -- the
                // pair (Tq, nRel) below -- where the cap sits exactly ON
                // dirPrev. Only past neutral SHORTEN:
                // T = ceil(3*(v1 - gain)/(dirPrev*gShR)) raises the FC
                // bound to meet dirPrev at the shorten's own flight g
                // (gRel for front-loads, neutral for back-loads), so the
                // launch keeps its velocity and decelerates smoothly
                // into the target, landing early. The ride keeps the RAW
                // g (relAdapt gates on liftAhead == 0, and the divisor
                // arm keeps gRel there): a mid-ride relax would
                // mis-scale the stored boosted launch, whose safety
                // lives in rideMax + the per-sample induction, not in
                // the cap. All branches agree at their boundaries
                // (dirPrev == 3*gap/(gRel*nRel) is capped's edge with
                // relT == nRel on both sides; dirPrev*nRel == 3*gap
                // gives Tsh == nRel == the adapt branch -- adapt is only
                // reachable at gRel > 1, where the divisor's gShR = 1),
                // launches from rest are untouched (capped false at
                // dirPrev ~ 0), and ceil keeps T integer so the leg
                // clock reaches exactly 1 (a fractional T parks a landed
                // leg shy of its target and deadlocks the v1 > p1
                // trigger). relGap can go <= 0 here (these values
                // compute every sample; term 3 can fire at v1 == gain):
                // the eps floor keeps the idle division finite and
                // max(1, ...) catches the negative branch -- a T = 1
                // stop, identical in output to an m0 = 0 clamp.
                relGap = v1-gain;
                capped = (dirPrev*nRelG)>(3*relGap);
                // ONE shared division (+ its ceil) serves BOTH shortened
                // legs AND both feasible-shape pairs: Tshort (release,
                // the max(eps, ...) branch) and attTs (attack, the
                // min(-eps, ...) branch) are consumed on opposite sides
                // of relTrig -- relT only through T0's release branch,
                // attT only through its attack branch -- so the shared
                // quotient reproduces whichever one is read, bit-exactly,
                // and the discarded branch's value (always finite:
                // |den| >= eps) is never consumed. The un-ceiled
                // quotient Tq doubles as the adapt pair's numerator: on
                // the adapt branches the divisor's g factor is exactly 1
                // (adapt is only reachable at g > 1, where min(g, 1)
                // = 1), so Tq = 3*gap/dirPrev and gEff = Tq/T --
                // carried as the pair, never divided. The shorten
                // divisor keeps min(g, 1) -- the flight g of a
                // past-neutral shorten -- inside the eps clamp, keeping
                // the floor at eps itself; the release arm keeps the
                // RAW gRel while liftAhead (the ride's shorten is
                // untouched).
                shGap = select2(relTrig, attGap, relGap);
                shDir = select2(relTrig, min(0-ma.EPSILON, dirPrevP*gShA), max(ma.EPSILON, dirPrev*select2(liftAhead, gShR, gR)));
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
                Tq = g3/shDir;
                Tsh = ceil(Tq);
                relAdapt = capped&(liftAhead==0)&((dirPrev*nRel)<=(3*relGap));
                relT = select2(capped, nRel, select2(relAdapt, max(1, min(nRel, Tsh)), nRel));
                // the same medicine, mirrored onto flat-chord attacks: a
                // leg whose landing chord is flat (the critical candidate
                // IS the window-deepest -- its next-deeper sentinel copies
                // its own value, an exact compare, no tolerance) and whose
                // entry arrives hotter than the full-deadline FC floor
                // (dirPrev * gAtt * critDl < 3 * (critVal - gain),
                // cross-multiplied, both sides negative there) first
                // RELAXES its warp (attAdapt): while the entry fits the
                // NEUTRAL floor (dirPrev*critDl >= 3*gap, i.e. the
                // feasible g = 3*gap/(dirPrev*critDl) >= 1, reachable
                // only at gAtt > 1), fly the whole deadline at that g --
                // the pair (Tq, critDl) below -- where the floor sits
                // exactly ON dirPrev: the leg lands ON TIME instead of
                // early, which is the point (v1.5.0 turned high attack
                // shapes into early bottoms and long flats exactly
                // here). Only past neutral SHORTEN:
                // attT = ceil(3*gap/(dirPrev*gShA)) puts the floor
                // exactly ON dirPrev at the shorten's flight g, so the
                // entry keeps its velocity, decelerates smoothly, lands
                // at v1 EARLY and holds flat through the pin's play
                // sample. Early landing at the window-deepest is safe by
                // construction (every candidate value is >= v1: nothing
                // binds until the pin plays out), and the mirrored
                // no-undershoot lemma holds at the FC boundary with
                // equality, so gain >= v1 survives -- m0Tt lands exactly
                // ON 3*(p1 - p0) on both branches: on adapt the raw-g
                // launch arm dirPrev*gAtt*Tt sits past the floor BY THE
                // attHot GATE (Tt = critDl there), so max(g3, .)
                // saturates to g3 with no pair arithmetic at all. Cool
                // entries and sloped chords fly T = critDl untouched
                // (all branches agree at their edges: the attHot edge
                // lands on the adapt branch at gAtt > 1 and on
                // Tsh == critDl at gAtt <= 1; the adapt/shorten edge on
                // Tsh == critDl), so block S-curves are unaffected; the
                // trigger algebra stays live throughout, so nothing is
                // blinded -- see the header. The eps floor keeps the
                // idle division finite when dirPrev reads >= 0 (that
                // branch is discarded by the attHot gate, which is false
                // for any dirPrev >= 0: hold and rising entries launch
                // as before).
                attGap = critNum;
                dlF = 1.0*critDl;
                flatChord = critNpV==critVal;
                attHot = flatChord&(((dirPrevP*gA)*critDl)<(3*attGap));
                attAdapt = attHot&((dirPrevP*critDl)>=(3*attGap));
                // THE RISING-ENTRY FEASIBLE SHAPE (v1.8.0, see the
                // header). Crest bound for a rising launch: with
                // a = m0T, D = gain - critVal (= -critNum, free off
                // the tree, > 0 on any attack), the crest above the
                // launch point is E(a) = a^2(4a+9D)/(27(a+2D)^2)
                // (exact at m1T = 0, an upper bound for m1T <= 0),
                // and E(a) <= H is implied by a <= aMax below --
                // one sqrt, no division. H = headroom against the
                // sample playing NOW (tap 0), floored at 0; the
                // sqrt argument is guarded per the porting rules
                // (D < 0 on discarded release-branch evaluations).
                // aMax >= 0 always, so min(., aMax) is a bit-exact
                // no-op on every arm that is <= 0 (rest and
                // descending entries) and on every rising entry
                // whose requested crest already fits.
                attHeadroom = max(0.0, val0-gain);
                aMax = 3.0*attHeadroom+sqrt(max(0.0, attHeadroom*(9.0*attHeadroom+(0.0-12.0*critNum))));
                // the gate: the requested plan's crest exceeds the
                // headroom. dpT = dirPrevP*critDl > 0 on any taken
                // branch (the governed entry since v1.9.1): critDl = 0
                // or dirPrevP <= 0 read the gate false (aMax >= 0,
                // strict >), so the pair's denominator never goes 0
                // where it is consumed.
                dpT = dirPrevP*dlF;
                attRise = (dirPrevP>0)&((gA*dpT)>aMax);
                attT = select2(attHot, critDl, select2(attAdapt, max(1, min(critDl, Tsh)), critDl));
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
                // approach governor: velocity <= 3*(v1 - gain)/(gRel*nRel),
                // fast
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
                rideK = (3.0*invGR)/(float(nRel)*float(nRel));
                // aim IS rideMax = (v1 - gain)/(i1 + 1) on this branch:
                // the shared quotient above, release side. The x Tt scale
                // is applied OUTSIDE the min/max (bitwise-identical:
                // scaling by Tt > 0 is monotone and rounds monotonically),
                // which also keeps aim's quotient behind a min/max barrier
                // -- Faust's normal form reassociates a bare (n/d)*T into
                // (n*T)/d, splitting the shared division in two. The warp
                // rides the same scale: physical launch slopes latch as
                // slope*T*g (TtG), physical landing slopes as slope*T/g
                // (TtIG), both branch-shared through gF (= gCk, swung to
                // 1.0 on an engaged relax) like the quotient itself.
                TtG = Tt*gF;
                TtIG = Tt*invGF;
                // dT stays the RAW entry (the release floor aBT reads
                // it); dTA is the governed attack arm (v1.9.1)
                dT = dirPrev*TtG;
                dTA = dirPrevP*TtG;
                rideT = min(dirPrev+(nhV-v1)*rideK, aim)*TtG;
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
                // covering. Under the warp this monotonicity is the ONE
                // lemma that does not transfer whole: exact at g = 1,
                // still holding for rest and descending entries at
                // g >= 1, failing only in the corners listed in THE
                // SHAPE WARP (header) -- bounded, |g - 1|-small,
                // backstopped. Two regimes, split on the entry direction:
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
                //   sufficient condition u >= sqrt(s) with
                //   s = (gain - val)/(gain - p1t), hence
                //   Tclr^2 = (gain - p1t) * minFailed(dl^2/(gain - val)),
                //   the min run cross-multiplied as a (num, den) pair
                //   tree: ONE division + ONE sqrt total, ceil'd integer
                //   like Tshort. The condition is a u-space fact pulled
                //   back at the flight's own clock: g <= 1 needs no
                //   scale (wInv(x) <= x), and an engaged g > 1 leg flies
                //   NEUTRAL (relaxCk -- see the header), where wInv is
                //   the identity -- exact, no zClr factor since v1.6.0.
                //   Overshortens by at most sqrt(3) in the
                //   small-gap limit -- the safe
                //   direction. A failed
                //   candidate always has gain > val > p1t here (val > p1t
                //   by the argmin, val < gain since p(dl) <= gain for
                //   m0 <= 0), so s lands in (0, 1] and the den floor is
                //   idle; a hump-class failure (val >= gain, only
                //   reachable with m0 > 0) reads unconstraining in this
                //   branch by the same floor -- the chop class owns it.
                // Per-sample re-latching and the deadline clamp remain
                // the backstop for what the dyadic argmins shadow. Cost:
                // ~11 multiplies and two compares per candidate
                // (homogeneous Horner,
                // shared coefficients; ~5 unwarped) plus ~3
                // more for the two trees, one division, one sqrt, one
                // ceil; no state.
                ckT = 1.0*T0;
                ckTG = ckT*gCk;
                ckTIG = ckT*invGCk;
                // the crest cap rides both the plan and the check:
                // on attRise the raw-g arm sits past aMax BY THE
                // GATE, so the min saturates to aMax exactly; on
                // rest and descending entries the arm is <= 0 <=
                // aMax, a bit-exact no-op. The release branch of
                // ckM0 is never consumed (engagement gates on
                // relTrig == 0), so the min rides unconditionally.
                ckM0 = max(g3, min(dirPrevP*ckTG, aMax));
                ckM1 = select2(relTrig, max(g3, min(0, aim)*ckTIG), 0);
                // the check cubic under the warp, contracted
                // homogeneously. At deadline df the leg clock reads
                // u = df/DD with DD = df + g*(ckT - df), g = gN1/gD1
                // the pre-engage flight pair (see the header); clearing
                // DD^3 -- carried as DDs = DD*gD1, dfs = df*gD1, valid
                // by the same homogeneity, gD1 > 0 -- keeps the test
                // division-free:
                //   pl = gain*DD^3 + ckM0*(df*DD^2)
                //        + (g3 - 2*ckM0 - ckM1)*(df^2*DD)
                //        + (2*(gain - p1t) + ckM0 + ckM1)*df^3
                // in Horner form over dfs, tested against valC*DDs^3 --
                // the warped p(u(df)) <= val exactly, play-time meaning
                // intact. The df-free factors depend only on the latch,
                // not the candidate, so they are computed ONCE per
                // sample; each candidate pays dfs, DDs, its powers and
                // the Horner (~12 multiplies; ~5 unwarped). Plain
                // latches carry gD1 = 1.0, so dfs = df exactly and DDs
                // reproduces the scalar DD bit-for-bit; at g = 1,
                // DDs = df + (ckT - df) = ckT EXACTLY -- deadlines and
                // T0 are integer-valued doubles, every partial sum is
                // integer -- so each product's operands, hence the whole
                // Horner and the bound, reproduce the unwarped check
                // bit-for-bit. Re-association moves the
                // rounding at the pass boundary only: a flip there shortens
                // or relaxes T by rounding noise, inside the check's own
                // slack, and the deadline clamp + per-sample re-latch
                // backstop both directions as always.
                ckC0 = g3-2*ckM0-ckM1;
                ckD0 = 2*(gain-p1t)+ckM0+ckM1;
                clearOne(val, dl, npv, npd) = num, den, dlP
                    with {
                        df = 1.0*dl;
                        df2 = df*df;
                        valC = min(val, 2.0);
                        dfs = df*gD1;
                        DDs = dfs+gN1*(ckT-df);
                        D2 = DDs*DDs;
                        D3 = D2*DDs;
                        pl = gain*D3+dfs*(ckM0*D2+dfs*(ckC0*DDs+dfs*ckD0));
                        pass = (dl<=0)|(df>=ckT)|(pl<=(valC*D3));
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
                // no zClr: an engaged flight at g > 1 has been relaxed
                // to neutral, where the wInv pull-back is the identity;
                // g <= 1 never needed a scale (see the header).
                Tneg = ceil(sqrt(max(0.0, (gain-p1t)*(clrPair:(_, !))/(clrPair:(!, _)))));
                Tpos = checks:par(i, nC, (!, !, _)):minTree(nC);
                Tclr = select2(dirPrevP>0, Tneg, Tpos);
                // Tpos doubles as the clear flag: every failed candidate
                // contributes dlP = df < ckT <= 1e30 and every passing one
                // the 1e30 sentinel, so "some check failed" == Tpos < 1e30,
                // exactly -- no separate pass AND-tree is needed.
                engaged = (relTrig==0)&(Tpos<1e30);
                Tt = select2(engaged, T0, max(1, min(T0, Tclr)));
                m0Tt = select2(relTrig,
                    max(g3, min(dTA, aMax)),
                    select2(liftAhead, aBT, max(aBT, rideT)));
                m1Tt = select2(relTrig, max(g3, min(0, aim)*TtIG), 0);

                TN = select2(trig, T, Tt);
                p0N = select2(trig, p0, gain);
                m0TN = select2(trig, m0T, m0Tt);
                p1N = select2(trig, p1, p1t);
                m1TN = select2(trig, m1T, m1Tt);
                // the leg's warp pair, latched at the trigger like T
                // (the pre-first-trigger (0, 0) is never consumed:
                // gliding is false until a leg is latched)
                gNN = select2(trig, gNL, gNt);
                gDN = select2(trig, gDL, gDt);
                // segments start at k = 1: first step on the trigger sample, so
                // per-sample re-triggers re-plan instead of stalling, and the
                // trigger sample keeps the previous PHYSICAL velocity
                // (p(w(1/T)) ~= p0 + m0T*w'(0)/T = p0 + m0T/(g*T))
                kN = select2(trig, min(k+1, TN+1), 1);

                // Hermite basis at the warped clock u = w(k/T),
                // w(t) = t/(t + g*(1-t)) folded into the division with
                // g = gNL/gDL as the latched pair:
                // u = k*gD/(k*gD + gN*(T-k)) -- one division, replacing
                // tau's, absorbing the pair's denominator (gD > 0). On
                // plain latches gD = 1.0, so k*gD = k exactly and the
                // scalar clock is reproduced bit-for-bit. The
                // denominator is >= k*gD >= 1 on the live range
                // (k <= T, gN > 0, gD >= 1); the max(1, ...) guards the
                // parked k = T+1 read like tau's did (its value can go
                // small or negative there, is clamped finite, and is
                // discarded by gliding). u reaches EXACTLY 1 at k = T:
                // k*gD + gN*(T-k) = k*gD + gN*0 = k*gD, numerator ==
                // denominator bitwise, so lands (u = 1) one sample
                // before the target plays, then holds it through the
                // play sample -- the integer-T landing argument,
                // unchanged. mixed int/float division is float in Faust.
                u = (kN*gDN)/max(1, kN*gDN+gNN*(TN-kN));
                t2 = u*u;
                t3 = t2*u;
                h00 = 2*t3-3*t2+1;
                h10 = t3-2*t2+u;
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
// _ : lookaheadAttackReleaseSmootherShaped(nAtt, nRel, gAtt, gRel, maxAtt) : _
// ```
//
// * `nAtt`: attack lookahead in samples (1 <= nAtt <= maxAtt, may
//   vary at control rate)
// * `nRel`: release leg length in samples (>= 1, may vary at control
//   rate); 1 = instant rises to the window min
// * `gAtt`, `gRel`: per-direction shape (> 0, may vary at control
//   rate; latched per leg) -- see THE SHAPE WARP in the header.
//   g < 1 front-loads a leg, g > 1 back-loads it; the unshaped
//   entry point IS the shaped one at gAtt = gRel = 1,
//   bit-identically.
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

lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, rawGR) = lookaheadAttackReleaseSmootherShapedCk(nAtt, nRel, 1, 1, maxAtt, checkEvery, rawGR);

lookaheadAttackReleaseSmootherShaped(nAtt, nRel, gAtt, gRel, maxAtt, rawGR) = lookaheadAttackReleaseSmootherShapedCk(nAtt, nRel, gAtt, gRel, maxAtt, checkEvery, rawGR);

// fully parameterized variants, for callers that want the knob as an
// argument instead of the constant above
lookaheadAttackReleaseSmootherCk(nAtt, nRel, maxAtt, checkEvery, rawGR) = lookaheadAttackReleaseSmootherShapedCk(nAtt, nRel, 1, 1, maxAtt, checkEvery, rawGR);

lookaheadAttackReleaseSmootherShapedCk(nAtt, nRel, gAtt, gRel, maxAtt, checkEvery, rawGR) = hermiteAttackReleaseFollower(nB+1, nRel, gAtt, gRel, checkEvery, cands)
    with {
        nB = int(floor(log(maxAtt)/log(2))+1);
        // the bank output is the follower's candidate list: (value,
        // deadline, npV, npD) for the attack window, then for every tap,
        // then the next-higher pair (nhV, nhD) from the release taps
        cands = rawGR:slidingMinIdxBankAtt(nAtt, maxAtt);
    };

//----------------`lookaheadAttackReleaseSmootherOs`---------------------
// The smoother with a TRANSIENT overshoot allowance in dB. Two
// constraint arms are combined by min() and fed to the plain smoother:
//
//   min(slowGR, min(0, rawGR + os)) : lookaheadAttackReleaseSmoother...
//
// * the CAP arm rawGR + os (clamped <= 0): the output may sit at most
//   `os` dB above the raw GR at any play time -- a HARD bound, by the
//   smoother's own brickwall induction (output <= its input's window
//   min at play times, unchanged).
// * the SLOW arm slowGR: the musical descent the gain settles along
//   once inside the band -- and the reason the parameter lives OUT
//   here: the follower's idle state HOLDS (there is no downward
//   drift), so a transient-only overshoot needs a settle RATE, and
//   that rate should stay a caller-owned signal (rawGR through a
//   fixed slow one-pole attack, say), not a constant hidden in the
//   core.
//
// Feeding the min() upstream loses nothing vs. relaxing candidates
// inside the follower: pointwise min factors through the sliding-min
// bank exactly -- every dyadic tap of the combined signal is the min
// of the two signals' taps, timestamps riding with the winners -- so
// the follower sees precisely the union constraint schedule, and
// every core lemma applies verbatim. The min() corner at the arm
// crossover is deliberately left in the INPUT: planning C1 legs
// through a cornered constraint schedule is exactly what the
// follower does.
//
// Neutral: os = 0 with slowGR = rawGR is the plain smoother for any
// GR <= 0 (min(x, min(0, x)) == x there); an os past the working
// depth reads the cap arm >= 0 >= slowGR, so the input is slowGR
// exactly (pure slow path). Releases and latency are untouched.
//
// #### Usage
//
// ```
// slowGR, rawGR : lookaheadAttackReleaseSmootherOs(nAtt, nRel, maxAtt, os) : _
// slowGR, rawGR : lookaheadAttackReleaseSmootherShapedOs(nAtt, nRel, gAtt, gRel, maxAtt, os) : _
// ```
//
// * `os`: overshoot allowance in dB (>= 0, control rate)
// * `slowGR`: the slow/settle gain path (<= 0 dB)
// * `rawGR`: the raw (full-depth) GR signal the cap is measured from
//----------------------------------------------------------------------
lookaheadAttackReleaseSmootherOs(nAtt, nRel, maxAtt, os, slowGR, rawGR) = lookaheadAttackReleaseSmootherShapedOs(nAtt, nRel, 1, 1, maxAtt, os, slowGR, rawGR);

lookaheadAttackReleaseSmootherShapedOs(nAtt, nRel, gAtt, gRel, maxAtt, os, slowGR, rawGR) = min(slowGR, min(0.0, rawGR+os)):lookaheadAttackReleaseSmootherShaped(nAtt, nRel, gAtt, gRel, maxAtt);

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

// --- Smoother parameters ---
// compile-time maximum: 50 ms at maxSR. Lower maxSR if you never run
// above 48/96k, to save memory and a few reduce stages.
maxSR = 192000;
maxAtt = int(0.05*maxSR);

relHoldMs = SmootherGroup(hslider("[0]rel hold[unit:ms][scale:log]", 50, 0.1, maxRelHold*1000, 0.1));
attMs = SmootherGroup(hslider("[1]attack lookahead [unit:ms][scale:log]", 42, 0.1, 50, 0.1));
attShapeSl = SmootherGroup(hslider("[2]attack shape", 0.69, 0, 1, 0.001));
attAucComp = SmootherGroup(checkbox("[3]att auc comp"));
relMs = SmootherGroup(hslider("[4]release [unit:ms][scale:log]", 42, 1, 1000, 1));
relShapeSl = SmootherGroup(hslider("[5]release shape", 0.69, 0, 1, 0.001));
relAucComp = SmootherGroup(checkbox("[6]rel auc comp"));
// AUC (loudness) compensation -- OPTIONAL, off by default, one box each.
// See THE AUC COMPENSATION in the header. The factor scales the DURATION
// (fold BEFORE the int()/clamp), derived from THIS smoother's warped-Hermite
// leg area, normalized so the sharpest shape = 1 (only ever shortens). When
// a box is off its factor is exactly 1.0, so nAtt/nRel revert bit-identically.
// The attack DIVES into reduction, so its accumulated reduction-area IS the
// leg area I(g) -- feed gAtt directly. The release RECOVERS out of reduction,
// so its reduction-area is 1 - I(g) = I(1/g) (an exact identity) -- feed
// 1/gRel, which reflects front-load <-> back-load and holds duration * (deep
// time) constant the correct way round.
nAtt = max(2, min(maxAtt, int(attMs*0.001*ma.SR*aucLevelMultSwitched(attAucComp, gAtt))));
nRel = max(1, int(relMs*0.001*ma.SR*aucLevelMultSwitched(relAucComp, 1.0/gRel)));
// demo mapping onto the warp g (see THE SHAPE WARP in the header):
// 0 is the unshaped smoother, bit-identically (pow(4, 0) == 1.0
// exactly). POSITIVE is the shapedSmoother-flavored direction for
// each leg family -- attack: late dive (g = 4^s > 1), release: fast
// initial recovery (g = 4^-s < 1) -- negative mirrors it. |s| = 1
// gives a 16:1 endpoint-velocity skew (w'(1)/w'(0) = g^2). The map
// is the demo's choice, not the core's: the API takes raw g, and a
// perceptual match to the shapedSmoother family (velocity-peak
// position) can be layered on top later. (Since v1.6.0 the
// requested g is a per-leg ceiling: hot entries and engaged legs
// degrade toward neutral in SHAPE, not duration -- see THE
// FEASIBLE SHAPE in the core's header.)

// Shape slider base: g = shapeBase^s, so |s| = 1 gives a
// shapeBase^2 : 1 endpoint-velocity skew. Must stay > 1.
shapeBase = 4;
// shapeBase = SmootherGroup(hslider("[2a]shape base", 8, 4, 16, 1));

gAtt = pow(shapeBase, attShapeSl);
gRel = pow(shapeBase, 0-relShapeSl);

// --- AUC (loudness) area factor ---------------------------------------
// The shape here is a Moebius time-warp w(t) = t/(t + g*(1-t)) on a cubic
// Hermite leg -- a DIFFERENT curve family from shapedSmoother's
// cheapCurveBase, so shapedSmoother's aucLevelMult / auc_poly.lib do NOT
// transfer (they would cancel the wrong area). The factor is derived from
// THIS smoother's own from-rest leg value(tau) = h01(w(tau)),
// h01(u) = 3u^2 - 2u^3, whose area over [0,1] is, in closed form,
//   I(g) = (2g^3 - 6 g^2 ln g + 3 g^2 - 6 g + 1) / (g-1)^4
// (verified to machine precision against a 4e5-point midpoint integral at
// g in {1/4, 1/2, 2, 4}). At g = 1 the (g-1)^4 denominator is 0/0 -> 1/2;
// near g = 1 the closed form loses conditioning (the (g-1)^4 amplifies), so
// the neutral band |g-1| < 0.1 uses the exact Taylor series about g = 1
//   1/2 - d/5 + d^2/10 - 2 d^3/35 + d^4/28 - d^5/42 + d^6/60 - 2 d^7/165,
//   d = g - 1
// which is accurate to < 1e-10 there and matches the closed form at the
// switch to ~1e-10 (smooth, no step). aucArea(g) picks the branch.
//
// select2 evaluates BOTH arms and blends arithmetically, so the closed
// form's 0/0 at g = 1 would poison the result (nan*0 = nan) even when the
// series arm is selected. gSafe forces g out of the neutral band before it
// reaches the closed denominator -- harmless, since the closed arm is only
// ever SELECTED when |g-1| >= 0.1, so the remapped neutral values are dead.
aucAreaClosed(g) = (2.0*gs*gs*gs-6.0*gs*gs*log(gs)+3.0*gs*gs-6.0*gs+1.0)/((gs-1.0)*(gs-1.0)*(gs-1.0)*(gs-1.0))
    with {
        gs = select2(abs(1.0-g)<0.1, g, 1.1);
    };
aucAreaSeries(g) = 0.5+d*(c1+d*(c2+d*(c3+d*(c4+d*(c5+d*(c6+d*c7))))))
    with {
        d = g-1.0;
        c1 = -1.0/5.0;
        c2 = 1.0/10.0;
        c3 = -2.0/35.0;
        c4 = 1.0/28.0;
        c5 = -1.0/42.0;
        c6 = 1.0/60.0;
        c7 = -2.0/165.0;
    };
aucArea(g) = select2(abs(1.0-g)<0.1, aucAreaClosed(g), aucAreaSeries(g));
// Normalize so the SHARPEST shape the slider reaches (g = 4, the minimum
// area) maps to 1 -- the factor is then <= 1 for every g in [1/4, 4], so
// compensation only ever SHORTENS a duration (the maxAtt budget can only
// get colder, never grows an allocation). Both attack and release slider
// ends reach g = 4 or g = 1/4; I is monotone in g and I(4) is the min.

// Sharpest shape the slider reaches is g = shapeBase (min area);
// g ranges over [1/shapeBase, shapeBase] and I is monotone in g.
aucAreaSharp = aucAreaClosed(shapeBase);
aucLevelMult(g) = aucAreaSharp/aucArea(g);
// Branchless switched blend, exactly as shapedSmoother: on in {0,1} so the
// off path is bit-exact 1.0 (durations revert bit-identically, preserving
// the g = 1 neutral guarantee); factor clamped to [0,1] so on*(m-1) can't
// blow up. Slider-rate: Faust hoists it out of the audio path.
aucLevelMultSwitched(on, g) = 1.0+on*(max(0.0, min(1.0, aucLevelMult(g)))-1.0);

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
        loop(prevGain, prevRef) = gain, ref, holdGain
            with {
                // rawGain = gain_computer(1, thresh-attOvershoot, knee, level)*strength;
                rawGain = gain_computer(1, thresh, knee, level)*strength;
                holdGain = rawGain:releaseHold;

                coeff = select2(holdGain>prevGain, ba.tau2pole(gainAtt), ba.tau2pole(gainRel));
                smoothed = holdGain+(prevGain-holdGain)*coeff;
                gain = holdGain:onePoleSwitching(gainRel, gainAtt);

                onePoleSwitching(att, rel, x) = loop~_
                    with {
                        loop(yState) = (1.0-coeff)*x+coeff*yState
                            with {
                                coeff = ba.if(x>yState, ba.tau2pole(att), ba.tau2pole(rel));
                            };
                    };
                gainRel = interpolate_logarithmic(dv, endRelease, startRelease);
                gainAttDV = (prevGain-holdGain)/attOvershoot:max(0):min(1):gainAttDVmeter;

                attCurve(shape, dv) = select2(abs(shape)<2e-3,
                    pow(r, dv)*g-r*g// == (r^dv - r)/(1 - r), exact
                    ,
                    1-dv)// r -> 1 limit
                    with {
                        r = pow(1000, shape);
                        den = 1-r+select2(abs(1-r)<ma.EPSILON, 0, ma.EPSILON);
                        g = 1/den;
                    };
                gainAtt = 0;
                //maxAttDJ*attCurve(attShape, gainAttDV);

                // gainAtt = interpolate_logarithmic(gainAttDV, maxAttDJ+minAtt, minAtt)-minAtt;

                refAttackTime = 0;
                singleprecisionMAX = 3.402823466e+38;

                ref = (prevGain-transitionRange):min(0)*strength:si.onePoleSwitching(refRel, refAttackTime):refMeter;
                refRel = it.interpolate_linear(dv,
                    transitionTime,
                    singleprecisionMAX/128);
                fastGR = (prevGain-prevRef);
                dv = (fastGR/transitionRange):max(0):min(1):dvMeter;
            };
    };

interpolate_logarithmic(dv, v0, v1) = v0*pow(v1/v0, dv);

compressor(l, r) = l@latency*gain, r@latency*gain, (preGain:ba.db2linear)@(nAtt-1), gain
    with {
        preBoth = compression_gain_mono_db_auto(1, thres, 0, max(abs(l), abs(r)):ba.linear2db);
        preGain = preBoth:(_, !);
        preHold = preBoth:(!, _);
        gain = lookaheadAttackReleaseSmootherShapedOs(nAtt, nRel, gAtt, gRel, maxAtt, attOvershoot, preGain, preHold):grMeter:ba.db2linear;
        latency = nAtt-1+rel_hold_samples;
    };

process = MainGroup(compressor);

demoGR = MainGroup(demo(testSignal))
    with {
        demo(rawGR) = grPlay, smoothed
            with {
                grPlay = de.delay(maxAtt-1+maxRelHoldSamples, nAtt-1+rel_hold_samples, rawGR);
                smoothed = lookaheadAttackReleaseSmootherShaped(nAtt, nRel, gAtt, gRel, maxAtt, (rawGR:releaseHold));
            };
    };
