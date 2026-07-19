declare name "hermiteAttackReleaseSmoother";
declare version "0.13";
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
// * (v0.3) Release taps, mirror-aligned: a second read of the SAME
//   cascade, anchored at the opposite edge. The attack taps slide
//   every stage into place to share the playing edge (delay
//   nAtt - 2^i); the release taps are the raw stages themselves --
//   suffix j = the last 2^j samples of the window, all sharing the
//   window's newest sample -- zero delay lines, zero new stages.
//   Nested suffixes sample the CEILING SCHEDULE C(s) = min over
//   play [s, nAtt) at dyadic distances from the far edge: the shape
//   of the raw release. The bank condenses them into ONE next-higher
//   pair (nhV, nhD): the level the ceiling lifts to when the pinned
//   min plays out (the largest suffix excluding i1; dyadic
//   resolution can read HIGH -- the mirror of the
//   shadowed-second-deepest gap; ties where the min value recurs
//   later give nhV == v1, the flat sentinel), plus the play index
//   of that level's own min.
//   The two OBVIOUS follower uses both measured as REGRESSIONS on
//   the noisy workload and ship NEUTRALIZED (release lands flat,
//   T = nRel), with the chord endpoints still selected so the pair
//   stays live in the hot path for experiments:
//   - m1 = aim (land at v1 moving into the coming rise): under
//     per-sample re-latch only the FIRST step of each curve is ever
//     taken, and h11(1/T)*T ~= -1/T, so a positive landing slope
//     subtracts aim/T from every step of the chase -- up to the
//     entire early-step budget, since aim clamps at 3*delta. It
//     shapes an ending that re-latching never reaches. Stall
//     fraction 0.13 -> 0.20 at rel 50 ms, noise 0.5.
//   - T = max(nRel, i1 + 1) (land ON the pin's play sample so the
//     lift re-latch carries velocity): deliberately slows the
//     governor whenever the pin sits far out; stall 0.007 -> 0.06
//     at rel 10 ms, where it engages.
//   Why more information cannot fix the remaining crawls: at an
//   attack landing the gain is pinned AT v1 through its play sample
//   (it must be), so C1 forces velocity ~0 into every such lift --
//   the rebuild there is the constraint, not a knowledge gap. The
//   promising schedule-aware use is the DECELERATION side: the
//   governor brakes for v1 even when the pin plays out before the
//   gain could reach it. Capping the per-step velocity at
//   (v1 - gain)/(i1 + 1) -- inductively safe under per-sample
//   re-latch -- and chasing nhV's governor otherwise would spend
//   that wasted braking; it needs a re-latch guard for flown-whole
//   legs (a latched m0 above 3*delta can overshoot v1 mid-flight
//   when no re-latch corrects it). Built, guarded and measured in
//   v0.4.
// * (v0.4) The lift-aware ride. The v1-chase brakes as its gap
//   closes (the drive term 3*(v1 - gain)/T^2 vanishes) -- wasted
//   braking whenever the pin plays out before the gain could reach
//   it anyway. While a strictly higher level is scheduled
//   (liftAhead = nhV > v1), the launch may keep the boosted chase
//   velocity -- dirPrev plus the farther level's drive,
//   (nhV - v1) * 3/nRel^2 per sample, a control-rate constant --
//   capped by the fastest rate that cannot cross v1 before the
//   lift: rideMax = (v1 - gain)/(i1 + 1). Safety is per-sample
//   INDUCTION, not a curve property: one step at v <= rideMax
//   leaves gain <= v1, and the trigger re-plans every sample while
//   liftAhead (term 2) or while a relaxed launch is latched
//   (term 3, the flown-whole guard: m0*T > 3*(p1 - p0), computable
//   from latched state alone, release legs only, with a 1e-8
//   relative margin so FC-capped launches never trip it). Both new
//   terms yield to attNeed -- unlike v1 > p1 they do not imply
//   v1 > gain, and a missed attack is a brickwall leak. Isolated
//   single-step rises never see liftAhead (the post-lift window is
//   single-level, nhV == v1), so the calibrated exact-nRel block
//   S-curve is preserved bit-identically; on multi-level material
//   the ride deviates by design (measured: every differing blocks
//   sample sat inside a multi-level-lookahead episode or its nRel
//   tail; blocks brickwall stayed exactly 0). Measured on noise
//   0.5: stall fraction 0.13 -> 0.07 at rel 50 ms (crawl median
//   13.2 -> 8.7 ms, p95 20.7 -> 13.4 ms); deficit mean
//   0.032 -> 0.020 and brickwall 1.6e-3 -> 6.7e-4 at rel 10 ms;
//   brickwall unchanged at 3.5e-4 at rel 50 ms. Accepted corners,
//   both bounded: a pin that RECURS at the wall (nhV read high in
//   the dyadic blind spot, or the i1 = 0 rounding, leak
//   <= 3*gap/T^2, orders below the hump class) parks the ride at
//   v1 with a velocity chop instead of a lift; attacks that fire
//   mid-ride pick up the hotter dirPrev, growing their hump within
//   the documented class. The crawls that remain are the C1
//   rebuild from zero after attack landings -- constraint-bound
//   (see v0.3); only a launch-shape change could shrink those.
// * (v0.5) rideOn: a compile-time 0/1 on the follower and the
//   wiring. 0 constant-folds the two v0.4 trigger terms and the
//   ride to dead code -- the compiled follower is the exact
//   pre-mirror (v0.2) plain chase, verified bit-identical on all
//   test renders (noise at rel 50 and 10 ms, blocks). The demo grew
//   a third channel: the old, non-mirrored-window smoother next to
//   the ride, for scope and listening A/B. Both followers read the
//   SAME bank -- Faust CSE shares the cascade, its delay lines and
//   the candidate scoring, so sizeof(dsp) stays 2.44 MB; the second
//   follower costs the demo ~36 ns/sample (state loop, trigger and
//   Hermite evaluation are per-instance). Library users instantiate
//   once: single-instance cost is unchanged.
// * (v0.6) Momentum-preserving release re-latch. Both re-latch
//   paths used to CLAMP the launch velocity onto their caps: term 1
//   onto the fresh-leg FC cap 3*(v1 - gain)/nRel -- any window-min
//   creep past a flying leg's midpoint lands there, since for a
//   whole leg re-latched at phase tau, cap/velocity =
//   (1-tau)(1+2tau)/(2tau) < 1 for tau > 1/2 (measured x0.42 at
//   tau = 0.75) -- and term 2 onto max(FC cap, rideMax) the sample
//   a lift blip appears (nhV one noise-hair above v1 flips
//   liftAhead while a leg is hot; measured x0.59). Both are a
//   one-sample velocity corner: the "kink on the release" on noisy
//   material. Fix: when dirPrev exceeds the fresh-leg cap
//   (dirPrev*nRel > 3*(v1 - gain), cross-multiplied, no division on
//   the compare), SHORTEN the leg instead of slowing the launch:
//   T = ceil(3*(v1 - gain)/dirPrev) puts the FC bound exactly at
//   dirPrev, so the launch keeps its velocity and decelerates
//   smoothly into the target, landing early rather than braking
//   instantly. The no-overshoot lemma holds at m0 = 3*delta with
//   equality, so gain <= v1 survives unchanged. At the governor
//   boundary Tshort == nRel: the select2 introduces no step of its
//   own, the steady creep governor is untouched, and launches from
//   rest (dirPrev ~ 0) plus whole-leg block S-curves stay
//   bit-identical (capped false there). ceil keeps T integer -- a
//   fractional T never reaches tau = 1, parking a landed leg
//   epsilon shy of its target and deadlocking the v1 > p1 trigger
//   -- and puts m0*T exactly ON 3*(p1 - p0), inside the
//   flown-whole guard's 1e-8 margin, so term 3 stays quiet on
//   these launches. Side effect: the ride's park-at-v1 velocity
//   chop (recurring pin) becomes a flat landing, since the
//   deceleration now starts when the cap first binds instead of at
//   the wall. Cost: one audio-rate division + ceil on the trigger
//   path (the plain follower gains its first division there).
// * (v0.7) momentumOn: a compile-time 0/1 on the follower and the
//   wiring, mirroring rideOn. 0 constant-folds the v0.6
//   shortened-leg re-latch away (capped == 0 pins relT = nRel and
//   Tshort to dead code) -- the exact v0.5 follower, verified
//   bit-identical on the noise and block renders. The demo's third
//   channel is now that (rideOn = 1, momentumOn = 0): the release
//   kink on the scope next to its fix, replacing the rideOn = 0
//   plain chase as the A/B reference.
// * (v0.8) Release launch floor. The launch tangent was
//   velocity-continuous in BOTH directions: m0 = min(3*delta,
//   dirPrev), a negative dirPrev launching a "safe, downward dip"
//   (brickwall-safe -- h10 >= 0 keeps the flight <= v1 regardless).
//   But a release targets v1, BY DEFINITION the deepest point in
//   the total lookahead: nothing below it exists to descend toward,
//   so any dip is gratuitous over-reduction. And the dip is
//   reachable: an attack re-latch chain lands ON its pin's play
//   sample (deadline 0, no hold sample in between), the pin leaves
//   the window on the NEXT sample, and term 1 fires with dirPrev =
//   the final approach step -- or worse, a hump's deadline-0 clamp
//   step. With T = nRel the Hermite integrates that tangent into a
//   swoop of up to ~0.15*T*|dirPrev| below the bottom (measured on
//   the noise workload: excursions to 0.85 below the window min,
//   hundreds of samples long; which follower showed a given event
//   was only the one-sample phase of its re-latch cadence). Fix:
//   floor the launch at 0 -- m0 = min(3*delta, max(0, dirPrev)).
//   Momentum re-latches (dirPrev > 0) and launches from a hold
//   (dirPrev == 0) are untouched, so block S-curves and the v0.6
//   machinery render bit-identical; the dip is replaced by a flat
//   launch whose one-sample velocity corner is the approach step
//   that was already there. The floor is its own compile-time
//   flag (floorOn, on the follower and the wrapper) so either
//   follower can A/B it; the demo's out3 disables it as the
//   undershoot reference. Invariant with the floor on: the gain
//   never DESCENDS below the current window min -- attack flights
//   stay >= their target >= v1 (FC box, or all-nonnegative Hermite
//   terms when m0 > 0), release flights now stay >= their launch
//   point.
// * (v0.9) flatK: flat landing at a pinned bottom (prototype).
//   The follower hugs the window-min curve on the way down --
//   dyadic legs each landing at the local chord slope, the creep
//   endgame clamping the last pins -- so it ARRIVES at the bottom
//   at the descent's terminal slope (<= 2.4e-4/sample across the 38
//   noise-render events). v0.8 zeroes that direction at the release
//   launch: a first-order corner of that size. flatK zeroes it
//   BEFORE arrival: within flatK samples of a FIXED pin (deadline
//   shrinking, i1 < i1') that the gain is still descending toward,
//   the critical candidate is overridden with candidate 0 itself --
//   whose landing chord is flat by construction -- and the ordinary
//   attack law flies ONE leg to (v1, i1) with m0 = dirPrev, m1 = 0:
//   direction zero AT the deepest point. The measured window-min
//   descent is CONVEX (it decelerates into its bottom; the mean
//   chord to the bottom ran 0.85-0.99x the critical requirement all
//   the way in on the deep noise events), so a flat-landing leg
//   necessarily flies BELOW the remaining constraint path -- it
//   sheds the same altitude with zero terminal speed. That is the
//   price: bounded extra reduction during the last flatK samples of
//   a descent. It is also why the first cut of this feature (fire
//   on 'the bottom chord TIES the critical requirement', correct
//   for straight ramps) never fired on the real material and
//   rendered nearly unchanged. Below the constraint no pin binds,
//   so the leg glides whole; a deeper min appearing mid-flight
//   re-fires onto the new bottom like any attack. The entry is the
//   condition's own rising edge, forced into attTrig: it fires
//   mid-flight where dirPrev IS the flight velocity, keeping the
//   entry C1 -- NOT gated on dirPrev < 0, because triggers fire at
//   rest and rest reads dirPrev = 0 (the landing hold sample and
//   the creep zipper's hold phase both blocked the first cut of
//   this feature on exactly the samples its trigger could fire).
//   Hold entries launch flat; if the constraint locally outruns
//   the S-curve, the backstop clamps once and the next `arrived`
//   re-latches with the clamp's velocity -- every latched leg
//   targets (v1, i1, m1 = 0), so the landing is flat on all paths. The wrapper
//   derives flatK = nAtt/4 from its flatLandOn flag; 0
//   constant-folds the whole block away -- out2 and out3 render
//   bit-identical with it off, blocks render bit-identical even
//   with it on (a step's approach leg already targets the bottom).
//   No division and no tolerance constant on the trigger path.
// * (v0.10) flatFullOn: full-lookahead flat landing -- the fix for
//   the v0.9 prototype's residual bottom kinks; v0.9 stays compiled
//   behind flatK as the A/B reference. Two v0.9 holes closed. (1)
//   The gate: `pinned` plus the nAtt/4 horizon abandoned the
//   commitment whenever the pin was replaced inside the horizon or
//   the endgame arrived before it, so some bottoms were still owned
//   by the hugging law's clamp chain and landed at its terminal
//   step. flatNeed = (v1 < gain) & (i1 > 0): whenever the gain is
//   descending toward the window-deepest point, the critical
//   candidate is overridden with (v1, flatT) and the leg lands
//   there with m1 = 0 -- detection of the deepest GR in the
//   lookahead IS the trigger, nothing else. Pin replacement needs
//   no special case: the override stays up and the ordinary
//   trigger algebra re-plans (critVal2 != p1 fires on every v1
//   step; the creep gate may fly a nearly-landed leg out first --
//   flat either way: a terrace, not a corner). (2) The entry: a
//   hugging chain arriving hotter than the full-window FC floor
//   (dirPrev * i1 < 3*(v1 - gain), cross-multiplied, both sides
//   negative) used to CLAMP onto m0 = 3*delta -- the mirror of the
//   v0.6 release corner, and a kink v0.9 could plant right at its
//   own flatK entry. Same medicine, mirrored: SHORTEN the leg,
//   flatT = ceil(3*(v1 - gain)/dirPrev), putting the FC floor
//   exactly ON dirPrev, so the entry keeps its velocity,
//   decelerates smoothly, lands at v1 EARLY and holds flat through
//   the pin's play sample (the mirrored no-undershoot lemma below
//   holds at m0 = 3*delta with equality, so gain >= v1 survives).
//   Cool entries fly T = i1 and land one sample before the pin
//   plays, as v0.9. Price, by design: the committed leg flies
//   below the hugging path over up to the whole window, not just
//   the last nAtt/4 -- more early reduction than v0.9 on long
//   approaches -- and intermediate candidates are hidden while the
//   override holds, trading the v0.3 hump class for a
//   flat-flight-over-intermediate-dip class (a dip shallower than
//   v1 playing under a still-high committed flight). Both costs to
//   be measured against v0.9 on the noise workload; a kink-free
//   bottom on every path is what they buy.
// * (v0.11) flatFullOn rebuilt: the v0.10 override, measured, IS
//   the bug. flatNeed = (v1 < gain) & (i1 > 0) also holds mid-rise
//   the moment a deeper block enters the window under a hot rising
//   launch, so the override replaced whatever the law had made
//   critical -- at the screenshot event a deadline-1 raw ceiling,
//   critDl 1 -> 1101 -- with the far window min, and the committed
//   leg flew straight through every hidden constraint for hundreds
//   of samples (peak +0.106 over the raw on the probe render; the
//   above-raw dive class ran 0.144 deep over 17k samples where
//   v0.9's ran 0.010 over 10k) before plunging to the far bottom
//   and releasing back up: the "blue overshoots" arc. The blinding
//   of intermediate candidates -- the v0.10 header's own listed
//   price -- is the failure; the info sat in the bank all along.
//   v0.11 deletes the override. Flat landing lives inside the
//   ordinary candidate law, which already paces every (value,
//   deadline) at every scale: the landing chord is flat EXACTLY
//   when the critical candidate is the window-deepest point (the
//   next-deeper sentinel copies the candidate's own value -- an
//   exact compare, no tolerance; candidate 0's chord is the flat
//   sentinel unconditionally), and m1 = 0 falls out of the aim
//   path untouched. The one thing the ordinary law was missing is
//   entry manners: a flat-chord leg latched hotter than its
//   full-deadline FC floor (dirPrev * critDl2 < 3 * (critVal2 -
//   gain), cross-multiplied, both sides negative there) CLAMPED
//   onto m0 = 3*delta -- the kink the reference channels plant at
//   the same spot. Same v0.6 medicine, mirrored: SHORTEN,
//   attT = ceil(3*(critVal2 - gain)/dirPrev), putting the floor
//   exactly ON dirPrev -- the entry keeps its velocity,
//   decelerates smoothly, lands at v1 EARLY and holds flat through
//   the pin's play sample. Early landing at the window-deepest is
//   safe by construction (every candidate value is >= v1, so
//   nothing binds until the pin plays out), and the mirrored
//   no-undershoot lemma holds at m0 = 3*delta with equality, so
//   gain >= v1 survives. Cool entries and sloped chords fly
//   T = critDl2 untouched (the select2 branches agree at the
//   boundary), so block S-curves stay bit-identical; the trigger
//   algebra stays live throughout, so a dip playing under any
//   flight re-latches like any attack -- nothing is blinded.
//   Bottoms the creep clamp chain still owns (the pin never
//   becomes critical before the endgame) land at the v0.8
//   terminal step: the residual corner class is v0.8's own,
//   orders below the two removed here. Cost: one division on the
//   latch path (attTs), the exact mirror of Tshort. The demo's
//   out2 slip is also fixed: it passed flatLandOn = 1, so out2
//   rendered bit-identical to out4 -- the screenshot's magenta
//   was a second v0.9, not the v0.8 reference its comment
//   promised; it is now the true (1, 1, 1, 0, 0).
// * (v0.12) The ceiling zipper, measured and killed. On smooth
//   descents (raw ramping down, a deep pin far out) the v0.11
//   probe render showed a limit cycle every ~6 samples: the law
//   correctly makes the far pin critical (required slope
//   -1.17e-3/sample at the traced episode, steeper than the local
//   ramp's -2.4e-4) and latches the correct flat-landing leg --
//   but the latch happens at `arrived`, one sample AFTER the
//   play-hold, where the previous clamp's velocity has already
//   died (dirPrev reads 0), so the T ~ 500 S-curve launches at
//   ~1e-5/sample, the ceiling outruns it within 4 samples, the
//   deadline-1 raw forces a T = 1 clamp (C0), one hold sample
//   zeroes dirPrev, repeat: a sawtooth riding ~1e-3 ABOVE the raw
//   (the brickwall leak class) with a |d2| ~ 1.4e-3 corner per
//   cycle -- "bumping into the ceiling", 193 corners > 1e-3 in the
//   17k-sample probe episode. Two terms fix it, both behind
//   flatFullOn:
//   - the LANDING HANDOFF: when deeper work is pending, attTrig
//     also fires on the first post-landing sample (k == T), where
//     dirPrev IS the landing step -- the chord aim (or a clamp)
//     finally hands its velocity to the next leg instead of dying
//     in the hold. attNeed gates it, so peak holds and release
//     holds are untouched, and descending through a shallower
//     pin's play sample is safe (gain <= its value throughout).
//   - the CLEARANCE-CHECKED LAUNCH: at every latch the planned
//     cubic is evaluated at every candidate deadline (T^3-scaled,
//     division-free, float-coerced -- d^3 overflows int32;
//     min(val, 2) caps the disabled-tap sentinel, safe since any
//     GR is <= 1); if any p(dl) > val, the m0 = max(lo, dirPrev)
//     plan is replaced by m0 = max(lo, critScore): launch at the
//     speed the schedule demands ON AVERAGE -- the steepest
//     requirement, so it outruns every shallower ceiling and the
//     crawl-clamp cycle cannot start. critScore is the argmin's
//     own score: no new division. Isolated block steps clear the
//     check and render untouched.
//   Model A/B over the worst episode: corners > 1e-3 193 -> 0,
//   p99.9 |d2| 1.3e-3 -> 2.5e-6, brickwall leak (+1.05e-3 peak,
//   784 samples) -> exactly 0, at 0.024 mean extra reduction
//   inside the episode -- the leg dives below the hugging path,
//   the safe direction. Cost: one compare (handoff) plus ~10
//   multiplies and two compares per candidate per sample (check),
//   no divisions, no state, no delay lines; everything folds away
//   at flatFullOn = 0, so out2/out4 render bit-identical.
// * (v0.13) C1 clearance: the checked launch's velocity jump,
//   measured and killed. The v0.12 boost REPLACED the launch
//   velocity whenever the check failed -- m0 = max(lo, critScore)
//   regardless of dirPrev -- a one-sample velocity corner at the
//   exact sample the plan first diverges from the reference. Three
//   entry classes on the noise probe (60 s, screenshot settings):
//   RISING entries (a deep pin entering the window under a hot
//   release leg) are CHOPPED onto the near-flat critScore --
//   |dv| up to 1.4e-3, the only |d2| > 1e-3 corners v0.12 had
//   left, and the screenshot's divergence kink -- while rest
//   entries jump 0 -> critScore (<= 1.3e-4) and falling entries
//   barely move (<= 1.7e-5). Same medicine as v0.6/v0.11, third
//   verse: keep m0 = max(lo, dirPrev) and SHORTEN the leg until
//   the planned cubic clears. Shortening is always the safe
//   direction: at fixed play time t, dp/dT =
//   2*tau*(1-tau)*t*(m0 - 3*delta)/T >= 0 (m0 >= 3*delta = lo by
//   construction, in every regime including m0 riding lo), so a
//   shorter leg only ever lowers the path -- failed candidates
//   are the only ones that need re-checking, passing ones stay
//   clear. Two regimes, split on the entry direction:
//   - dirPrev > 0 (the chop class): land EARLY. Tclr = the
//     smallest FAILED deadline. Every failed candidate moves into
//     the dl >= T auto-pass (the leg sits landed at p1 by then,
//     and every candidate due at-or-before critDl2 has val >=
//     critVal2 -- deeper-and-sooner would have won the argmin),
//     every passing one stays clear by monotonicity: exact, and
//     division-free (a min tree over select2(pass, dl, 1e30)).
//     The arc a rising entry needs in order to turn around C1
//     still flies -- compressed into the shortened leg, its peak
//     scaling with T -- instead of being chopped flat.
//   - dirPrev <= 0 (the zipper class): let CURVATURE do it. The
//     m0 = 0 cubic bounds every m0 <= 0 plan from above, and
//     q(u) = 3u^2 - 2u^3 >= u^2 gives the closed-form sufficient
//     condition tau >= sqrt(s), s = (gain - val)/(gain - p1):
//     Tclr^2 = (gain - p1) * min over failed of dl^2/(gain - val),
//     the min run cross-multiplied as a (num, den) pair tree --
//     ONE division + ONE sqrt total, ceil'd like Tshort.
//     Overshortens by at most sqrt(3) in the small-gap limit; the
//     safe direction again.
//   The engagement sample is unchanged (the exact check still
//   gates, now written with m0*T and m1*T as division-free
//   products so it no longer waits on delta), so the foresight is
//   kept: the plan still refuses, at the same sample, to fly a
//   schedule that would outrun it -- it now leaves the reference
//   path tangentially instead of with a corner. Measured on the
//   probe, A/B against v0.12: [PENDING VERIFICATION -- filled in
//   after the A/B render below].
//   Residual accepted classes, both backstopped by the deadline
//   clamp + per-sample re-latch exactly as v0.12's own shadowing
//   was: a rising entry's compressed arc can still poke above a
//   near candidate whose value sits above the launch point (the
//   documented hump class, shared with the reference), and a
//   failure the neg-branch bound reads as unconstraining
//   (gain - val <= 0 there) leaves Tclr at T0. Cost: the boost's
//   select2 is gone; in exchange one division, one sqrt and one
//   ceil on the latch path (the mirror of Tshort's) plus ~3
//   mults/candidate for the two trees.
// * A release leg cannot overshoot its target: with m1 = 0,
//   p - p1 = (1-tau)^2 * ((1+2*tau)*(p0-p1) + tau*T*m0)
//          <= (1-tau)^2 * (p1-p0) * (tau-1) <= 0
//   for any m0 <= 3*delta. Mirrored for attacks (p1 < p0): the same
//   identity gives p - p1 >= (1-tau)^3 * (p0 - p1) >= 0 for any
//   m0 >= 3*delta -- a committed flat-landing attack leg never
//   undershoots v1 (v0.10 leans on this at the FC boundary). Hence
//   m0 = min(3*delta, max(0, dirPrev)):
//   the same Fritsch-Carlson bound as the attack, mirrored -- a cap
//   instead of a floor -- plus the v0.8 launch floor at 0.
// * Attacks that fire mid-rise pick up OUR release leg's velocity as
//   dirPrev -- the attack-side v0.2/v0.3 pickup unchanged in form, now fed by the
//   internal release instead of an upstream one-pole. testSignal3
//   (upstream release) is kept only as an A/B reference.
//
// Cost, against the v0.4 numbers (attack-only, isolated algorithm,
// 115 ns/sample, sizeof(dsp) 2.4 MB): no new divisions (delta was
// already on the trigger path and is shared; the attack landing
// chord is untouched), no
// new state (same 7-wide loop), no new delay lines, cascade and bank
// verbatim. The audio hot path gains one compare (v1 > gain), one
// AND, one OR, four select2 and a min/max pair -- noise next to the
// per-candidate divisions and the argmin tree that dominate. nRel is
// slider-derived, so its guard runs in the control block. The wiring
// LOSES the ceilPlay tap (a maxAtt-long delay line). Latency
// unchanged: nAtt - 1. (Reasoned, not yet measured -- bench it.)
// v0.3 adds, all shared-cascade reads: two select2 chains of nB
// links plus nB window-exclusion compares in the bank (one
// next-higher pair total, not per candidate), and four select2 on
// the follower's trigger path -- measured +3.2 ns/sample on the
// full demo graph (191.4 -> 194.6, -double -O3 -march=native,
// identical generator), sizeof(dsp) unchanged. Still zero new
// divisions (the landing-chord division is shared between the
// attack aim and the release endpoints by selecting them first),
// zero new delay lines, zero new state. v0.4 adds one audio-rate
// division (rideMax) and ~15 cheap ops on the trigger path;
// interleaved best-of benches measure v0.4 at parity with v0.2
// within machine noise (249.8 vs 249.4 ns/sample on a loaded
// sandbox), sizeof(dsp) unchanged.
//
// Release semantics: fixed DURATION, not fixed time constant -- any
// rise, large or small, takes nRel samples from rest; a mid-flight
// re-latch that arrives hot keeps its velocity and lands early
// (T = ceil(3*gap/dirPrev) < nRel) instead of braking onto the
// fresh-leg cap (v0.6). An isolated peak fully
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
// * A ride into a pin that RECURS (nhV read high in the dyadic
//   blind spot, or the min value recurring past the largest
//   excluding suffix) parks at v1 instead of lifting -- since v0.6
//   as a flat landing (the shortened leg decelerates in) rather
//   than a velocity chop.
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
// _ : slidingMinIdxBankAtt(nAtt,maxAtt) : si.bus(4*(nB+1) + 2)
// ```
//
// * input: the RAW GR signal
// * `nAtt`: attack window length (1 <= nAtt <= maxAtt, may vary)
// * `maxAtt`: compile-time maximum (int)
// * out1..out4:            v1, i1, npFullV, npFullD (attack window)
// * out(5+4i)..out(8+4i):  tap i value, play idx, npV, npD
// * last two outs:         nhV, nhD -- the next-higher pair (v0.3):
//   the ceiling level after the pinned min plays out, and the play
//   index of that level's own min, read off the mirror-aligned
//   release taps (see the header)
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

        // --- the next-deeper chain --- (as v0.9; the top link degrades to
        // the flat sentinel: no lookahead beyond the window)
        // sV/sD: the tap clipped to the current window; a disabled tap's
        // clipped window IS the attack window
        sV(i) = select2(active(i), v1, tV(i));
        sD(i) = select2(active(i), i1, tD(i));
        npFullV = v1;
        npFullD = i1+1;
        // chain indexed from the top: k = 0 is the largest tap, whose
        // neighbour is the attack window; tap i sits at k = nB-1-i.
        // (v0.9's npKV(0) select2 has equal branches here, so it is gone.)
        npKV(0) = v1;
        npKD(0) = select2(v1<sV(nB-1), npFullD, i1);
        npKV(k) = select2(sV(nB-k)<sV(nB-1-k), npKV(k-1), sV(nB-k));
        npKD(k) = select2(sV(nB-k)<sV(nB-1-k), npKD(k-1), sD(nB-k));
        npTV(i) = npKV(nB-1-i);
        npTD(i) = npKD(nB-1-i);

        // --- release taps + next-higher chain --- (v0.3)
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
// latched Hermite attack legs (verbatim v0.4); between attack legs it
// rises toward the window min v1 with Hermite release legs of nRel
// samples, and HOLDS while v1 == gain.
//
// #### Usage
//
// ```
// hermiteAttackReleaseFollower(nC, nRel, rideOn, momentumOn, floorOn, flatK, flatFullOn, cands) : _
// ```
//
// Where:
//
// * `nC`: number of candidates (compile-time int)
// * `nRel`: release leg length in samples (>= 1, may vary at control
//   rate); 1 = instant rises to v1
// * `rideOn`: compile-time 0/1. 1 enables the v0.4 lift-aware ride;
//   0 constant-folds the ride machinery away entirely and compiles
//   to the exact pre-mirror (v0.2) plain-chase follower -- renders
//   bit-identical -- useful as an A/B reference.
// * `momentumOn`: compile-time 0/1. 1 enables the v0.6
//   momentum-preserving (shortened-leg) release re-latch; 0
//   constant-folds it away and compiles the v0.5 re-latch
//   (launch velocity clamped onto the fresh-leg FC cap: the
//   release kink) -- useful as an A/B reference.
// * `floorOn`: compile-time 0/1. 1 enables the v0.8 release launch
//   floor (never descend toward the deepest point); 0 constant-folds
//   the floor away and compiles the v0.7 two-sided launch, which can
//   swoop below the window min -- useful as an A/B reference for
//   exactly the undershoot the floor removes.
// * `flatK`: compile-time int, the v0.9 flat-landing horizon in
//   samples. 0 disables (constant-folds the block away). > 0:
//   within flatK samples of a fixed pin the follower commits to one
//   leg that lands there with m1 = 0 instead of hugging the
//   decelerating window-min all the way in and arriving with its
//   terminal slope -- see the v0.9 header. The wrapper passes
//   nAtt/4 when its flatLandOn flag is set.
// * `flatFullOn`: compile-time 0/1, the schedule-aware C1 pack.
//   1 enables three terms: the v0.11 flat-chord hot-entry shorten
//   (attT = ceil(3*gap/dirPrev): velocity kept, early flat landing
//   on the window min); the v0.12 landing handoff (re-latch on the
//   first post-landing sample while deeper work pends, so the
//   landing velocity carries instead of dying in the play-hold);
//   and the clearance-checked launch (the planned cubic is
//   evaluated at every candidate deadline; a plan the schedule
//   would outrun is SHORTENED, velocity kept -- v0.13; the v0.12
//   form launched at the critical's required slope instead, a
//   velocity corner at every engagement). 0 constant-folds all
//   three away. (The v0.10 critical-candidate override this line
//   once carried measured as a brickwall regression -- see the
//   header.)
// * `cands`: 4*nC + 2 signals: (value, deadline, next-deeper value,
//   next-deeper deadline) per candidate as in v0.9, then the
//   next-higher pair (nhV, nhD) from the release taps (v0.3). The
//   deadline is the exact play index; the next-deeper pair becomes
//   the attack landing chord (own value as next-deeper value = land
//   flat); the next-higher pair is selected into the shared landing
//   chord for release legs but currently NEUTRALIZED (release lands
//   flat -- the naive uses measured as regressions; v0.3 header).
//   Candidate 0 MUST be the full attack window: its value doubles as
//   the release target v1 and its deadline as the pin's play index
//   i1. Disabled candidates read (ma.MAX, 1, _, _) and never win.
//
// Per sample, every candidate gets (requiredSlope, value, deadline,
// npVal, npDl) with requiredSlope = (value - gain)/deadline; the
// critical candidate is the argmin (steepest descent required), and
// its chord (npVal - value)/(npDl - deadline) is the landing slope.
//
// Triggers:
// * attack: verbatim v0.4, creep gate included.
// * release: three-term trigger. v1 > p1 latches from rest
//   (gain == p1 there) and re-latches mid-leg whenever the window
//   min rises above the running target; a target that drops back is
//   flown past. Two v0.4 terms re-plan every sample -- while a lift
//   is scheduled (liftAhead) and while a relaxed launch is latched
//   (the flown-whole guard) -- both gated on attNeed == 0, so
//   attacks always win. Inert mid-attack-leg (v1 <= p1 until
//   touchdown).
// * idle (arrived, v1 == gain) holds gain. The landed target stays
//   the window min through its own play sample, so peaks are held
//   through the peak exactly as in v0.4.
//----------------------------------------------------------------------
hermiteAttackReleaseFollower(nC, nRel, rideOn, momentumOn, floorOn, flatK, flatFullOn, cands) = (loop~si.bus(7)):(_, si.block(6))
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
                // balanced min fold (v0.13: the chop-class Tclr -- the
                // smallest failed deadline)
                minTree(1) = _;
                minTree(2) = min;
                minTree(N) = (minTree(half), minTree(N-half)):min
                    with {
                        half = int(N/2);
                    };
                // balanced min fold over ratios num/den carried as pairs
                // and compared cross-multiplied, all dens > 0 -- no
                // division on the tree (v0.13: the zipper-class Tclr
                // bound)
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

                // ---- flat landing at a pinned bottom (v0.9, flatK > 0) ----
                // The horizon prototype, kept as the A/B reference: within
                // flatK samples of a FIXED pin (deadline shrinking,
                // i1 < i1') that the gain is still descending toward,
                // override the critical candidate with candidate 0 itself
                // -- landing chord flat by construction, so the ordinary
                // attack law flies one leg to (v1, i1) with m0 = dirPrev,
                // m1 = 0. See the v0.9 header for the full rationale and
                // the measured convexity bet. Residual kinks: the gate
                // abandons bottoms (pin replaced inside the horizon;
                // endgames that arrive before it), and a hot entry used
                // to clamp onto the FC floor -- the v0.11 shorten (below,
                // beside relT) now fixes that wherever the chord is flat,
                // including on this path. The entry is the condition's
                // own rising edge, forced into attTrig: it fires
                // mid-flight, where dirPrev IS the flight velocity,
                // keeping the entry C1, and deliberately has NO velocity
                // gate (`arrived` fires triggers on HOLD samples where
                // dirPrev reads 0; a hold entry launches m0 = 0, and if
                // the constraint locally outruns that S-curve the
                // per-sample backstop clamps once and the next `arrived`
                // re-latches with the clamp's velocity).
                // (The v0.10 flatNeed override that lived here measured
                // as a brickwall regression -- it blinded the law to
                // every intermediate candidate -- and is gone; see the
                // v0.11 header.)
                pinned = i1<i1';
                flatFire = (flatK>0)&pinned&(v1<gain)&(i1>0)&(i1<=flatK);
                flatEnter = flatFire&(1-flatFire');
                critVal2 = select2(flatFire, critVal, v1);
                critDl2 = select2(flatFire, critDl, i1);
                critNpV2 = select2(flatFire, critNpV, v1);
                critNpD2 = select2(flatFire, critNpD, i1);

                // ---- triggers ----
                arrived = k>T;
                // previous leg done, at rest
                attNeed = critVal2<gain;
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
                rRem = max(0, T-k);
                // remaining steps of the leg
                steeper = ((critVal2-gain)*rRem)<((p1-gain)*critDl2);
                flyOn = (steeper==0)&(critDl2>=rRem);
                // (v0.12) the landing handoff: a completed leg's velocity
                // used to die in the play-hold sample -- the leg lands (its
                // last glide sample carries the landing step, the chord aim
                // or a deadline clamp), the next sample HOLDS (dirPrev on
                // the sample after reads 0), and only then does `arrived`
                // re-latch: every flown-whole joint restarted from rest.
                // On a descending schedule that restart is a limit cycle
                // (see the v0.12 header: the ceiling zipper). When deeper
                // work is pending, re-latch on the first post-landing
                // sample instead (k == T: the previous sample produced
                // tau = 1), where dirPrev IS the landing step -- the aim
                // machinery finally hands its velocity to the next leg.
                // attNeed gates it: a landed WINDOW-DEEPEST pin reads
                // critVal2 == gain, so peak holds (and every release hold)
                // are untouched. Descending through a shallower pin's play
                // sample is safe: gain <= its value throughout.
                landed = flatFullOn&(k==T);
                attTrig = attNeed&(((critVal2!=p1)&(flyOn==0))|(critDl2<rRem)|arrived|landed|flatEnter);
                // release trigger, three terms:
                // 1) v1 > p1: latch from rest AND re-latch mid-leg whenever
                //    the window min rises above the running target (at rest
                //    gain == p1, so this reads v1 > gain; no trigger at
                //    v1 == gain: hold, which covers a landed attack target
                //    through its own play sample). (v0.2 -- see the header
                //    for the micro-leg staircase this fixed.) Implies
                //    v1 > gain, hence attNeed = 0, and is inert
                //    mid-attack-leg (v1 <= p1 until touchdown).
                // 2) (v0.4) liftAhead & (v1 > gain): while a strictly
                //    higher level is scheduled behind the pin, re-plan every
                //    sample so the ride cap (see m0t) stays inductively safe
                //    and its boost keeps feeding. Strict v1 > gain: a pinned
                //    gain must not latch (a flat latch would dip, m0 <= 0,
                //    below the pin).
                // 3) (v0.4) the flown-whole guard: a curve latched with a
                //    relaxed launch (m0 above the FC cap -- only the ride
                //    does this) must never fly uncorrected, since per-sample
                //    re-latch is what makes the ride safe. Detected from
                //    latched state alone: m0*T > 3*(p1 - p0), release legs
                //    only (p1 >= p0), with a 1e-8 relative margin so an
                //    FC-capped launch (m0 == 3*delta up to rounding) never
                //    trips it and block S-curves keep flying whole.
                // Terms 2 and 3 yield to attNeed explicitly: unlike term 1
                // they do not imply v1 > gain, and a missed attack is a
                // brickwall leak. With rideOn = 0 both terms fold away at
                // compile time: relTrig reduces to term 1 and the ride below
                // is dead-code-eliminated -- the exact v0.2 follower. A v1
                // that drops back under a flying release target is still
                // flown past; if the gain crosses it, attNeed catches the
                // crossing with a smooth arc.
                liftAhead = rideOn&(nhV>v1);
                relTrig = (v1>p1)|(rideOn&(attNeed==0)&((liftAhead&(v1>gain))|((p1>=p0)&((m0*T*0.99999999)>(3*(p1-p0))))));
                trig = attTrig|relTrig;

                // ---- new-segment values (only used when trig == 1) ----
                // (v0.6) momentum-preserving re-latch: a release re-latch
                // arriving with dirPrev above the fresh-leg FC cap
                // 3*(v1 - gain)/nRel used to clamp the launch onto that cap
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
                // in output to the old m0 = 0 clamp.
                relGap = v1-gain;
                capped = momentumOn&((dirPrev*nRel)>(3*relGap));
                Tshort = ceil((3*relGap)/max(ma.EPSILON, dirPrev));
                relT = select2(capped, nRel, max(1, min(nRel, Tshort)));
                // (v0.11) the same medicine, mirrored onto flat-chord
                // attacks: a leg whose landing chord is flat (the critical
                // candidate IS the window-deepest -- its next-deeper
                // sentinel copies its own value, an exact compare, no
                // tolerance) and whose entry arrives hotter than the
                // full-deadline FC floor (dirPrev * critDl2 <
                // 3 * (critVal2 - gain), cross-multiplied, both sides
                // negative there) is SHORTENED instead of clamped onto
                // m0 = 3*delta: attT = ceil(3*gap/dirPrev) puts the floor
                // exactly ON dirPrev, so the entry keeps its velocity,
                // decelerates smoothly, lands at v1 EARLY and holds flat
                // through the pin's play sample. Early landing at the
                // window-deepest is safe by construction (every candidate
                // value is >= v1: nothing binds until the pin plays out),
                // and the mirrored no-undershoot lemma holds at
                // m0 = 3*delta with equality, so gain >= v1 survives --
                // m0t*Tt lands exactly ON 3*(p1 - p0), like Tshort. Cool
                // entries and sloped chords fly T = critDl2 untouched
                // (both select2 branches agree at the boundary), so block
                // S-curves stay bit-identical; the trigger algebra stays
                // live throughout, so nothing is blinded -- see the v0.11
                // header. The eps floor keeps the idle division finite
                // when dirPrev reads >= 0 (that branch is discarded by
                // the attHot gate, which is false for any dirPrev >= 0:
                // hold and rising entries launch as before).
                attGap = critVal2-gain;
                flatChord = flatFullOn&(critNpV2==critVal2);
                attHot = flatChord&((dirPrev*critDl2)<(3*attGap));
                attTs = ceil((3*attGap)/min(0-ma.EPSILON, dirPrev));
                attT = select2(attHot, critDl2, max(1, min(critDl2, attTs)));
                T0 = max(1, select2(relTrig, attT, relT));
                p1t = select2(relTrig, critVal2, v1);
                delta = (p1t-gain)/Tt;
                // average slope, sign = direction
                // landing chord, ONE shared division: every attack landing
                // aims at the nearest strictly-deeper point one scale out
                // (the critical candidate's np pair; own value as np value =
                // land flat). The release endpoints select the next-higher
                // pair, keeping the chord live for schedule-aware
                // experiments, but the release LANDS FLAT (m1t below): the
                // naive aim measured as a regression -- see the v0.3 header.
                aimV2 = select2(relTrig, critNpV2, nhV);
                aimD1 = select2(relTrig, critDl2, i1);
                aimD2 = select2(relTrig, critNpD2, nhD);
                aim = (aimV2-p1t)/max(1, aimD2-aimD1);
                // Fritsch-Carlson bound: a launch floor on attacks
                // (delta < 0), a launch cap on releases (delta > 0)
                lo = 3*delta;
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
                // when far, gentle when near. (v0.8) The downward direction
                // is NOT picked up on releases: the target v1 is the deepest
                // point in the total lookahead, so a negative dirPrev (an
                // attack chain landing ON its pin's play sample, or a hump's
                // deadline-0 clamp step, with the release firing on the very
                // next sample) would swoop the gain below the bottom --
                // gratuitous over-reduction, ~0.15*T*|dirPrev| deep. The
                // launch is floored at 0 instead; attacks keep the two-sided
                // pickup (their targets legitimately lie below).
                //
                // (v0.4) the lift-aware ride. The v1-chase brakes as the gap
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
                // velocity chop instead of a lift; and attacks that fire
                // mid-ride pick up the hotter dirPrev, growing their hump
                // within the documented class. max(aB, ride) keeps the ride
                // never slower than the plain chase (and discards a negative
                // rideMax the same way).
                rideK = 3.0/(float(nRel)*float(nRel));
                rideMax = (v1-gain)/(i1+1);
                ride = min(dirPrev+(nhV-v1)*rideK, rideMax);
                // (v0.8) launch floor (floorOn = 1): never descend toward the
                // deepest point. floorOn = 0 keeps the v0.7 two-sided launch
                // as an A/B reference: it can swoop below the window min.
                aB = min(lo, select2(floorOn, dirPrev, max(0, dirPrev)));
                m0a = max(lo, dirPrev);
                // (v0.12, launch rebuilt in v0.13) the clearance-checked
                // launch. A slow entry -- rest, or the tail of a
                // decelerated leg -- against a DESCENDING schedule plans
                // a cubic whose early crawl is outrun by the ceiling: the
                // raw catches it within a few samples, a deadline-forced
                // clamp lands on the playing value, and the cycle
                // restarts (the zipper; v0.12 header). The info to refuse
                // that plan sits in the bank: evaluate the planned cubic
                // at EVERY candidate deadline and demand p(dl) <= val --
                // the play-time constraint, exactly, at the scales the
                // bank resolves. Division-free: multiply the Hermite
                // basis through by T^3, with m0*T and m1*T folded to
                // max(3*(p1t - gain), dirPrev*T) and its m1 mirror so the
                // check does not wait on delta (all factors coerced to
                // float first: d^3 overflows int32 at large windows).
                // Disabled candidates auto-pass (min(val, 2) caps the
                // ma.MAX sentinel and any GR is <= 1, so the capped bound
                // only ever relaxes a true one); deadlines outside (0, T)
                // auto-pass (dl >= T lands first and holds at p1 <= val;
                // dl <= 0 is the sample playing now).
                //
                // (v0.13) On failure the leg is SHORTENED, velocity kept
                // -- the v0.6/v0.11 medicine, third verse. (The v0.12
                // form instead launched at max(lo, critScore), the
                // argmin's own score: a one-sample velocity corner at
                // every engagement -- rising entries CHOPPED onto the
                // near-flat chord, the screenshot kink; rest entries
                // snapped from 0 to the chord.) Shortening is the safe
                // direction: at fixed play time t, dp/dT =
                // 2*tau*(1-tau)*t*(m0 - 3*delta)/T >= 0 since
                // m0 >= 3*delta = lo by construction in every regime, so
                // a shorter leg only ever lowers the path -- passing
                // candidates stay clear, only the failed set needs
                // covering. Two regimes, split on the entry direction:
                // * dirPrev > 0 (the chop class): land EARLY.
                //   Tclr = the smallest FAILED deadline: every failed
                //   candidate moves into the dl >= T auto-pass (the leg
                //   sits landed at p1 by then, and every candidate due
                //   at-or-before critDl2 has val >= critVal2 --
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
                // the backstop for what the dyadic argmins shadow, as in
                // v0.12. Cost: ~10 multiplies and two compares per
                // candidate plus ~3 more for the two trees, one division,
                // one sqrt, one ceil; no state; folds away with
                // flatFullOn = 0.
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
                        pl = gain*(2*df3-3*df2*ckT+ckT3)
                            +m0aT*(df*(ckT-df)*(ckT-df))
                            +p1t*(df2*(3*ckT-2*df))
                            +m1tT*(df2*(df-ckT));
                        pass = (dl<=0)|(df>=ckT)|(pl<=(valC*ckT3));
                        num = select2(pass, df2, 1e30);
                        den = select2(pass, max(1e-30, gain-valC), 1.0);
                        dlP = select2(pass, df, 1e30);
                    };
                checks = cands:(si.bus(4*nC), si.block(2)):par(i, nC, clearOne);
                clear = select2(flatFullOn,
                    1,
                    checks:par(i, nC, (_, !, !, !)):clrTree(nC));
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
                // a release leg, v1 < gain an attack), and rises are legs now
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
// _ : lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt,
//          rideOn, momentumOn, floorOn, flatLandOn, flatFullOn) : _
// ```
//
// rideOn: compile-time 0/1 -- 1 = the v0.4 lift-aware ride, 0 = the
// exact pre-mirror (v0.2) plain chase, ride machinery folded away.
// momentumOn: compile-time 0/1 -- 1 = the v0.6 momentum-preserving
// release re-latch, 0 = the v0.5 velocity-clamping re-latch.
// floorOn: compile-time 0/1 -- 1 = the v0.8 release launch floor,
// 0 = the v0.7 two-sided launch (can undershoot the window min).
// flatLandOn: compile-time 0/1 -- 1 = the v0.9 flat landing at a
// pinned bottom (prototype, horizon nAtt/4), 0 = hugging arrival
// at the window-min's terminal slope.
// flatFullOn: compile-time 0/1 -- 1 = the schedule-aware C1 pack:
// the v0.11 flat-chord hot-entry shorten, the v0.12 landing
// handoff (landing velocity carries into the next leg), and the
// clearance-checked launch (plans the schedule would outrun are
// SHORTENED with their velocity kept -- v0.13; v0.12 launched
// them at the critical's required slope, a velocity corner at
// every engagement). Kills the ceiling zipper and the flat-chord
// entry clamp; 0 folds all of it away. Replaces the v0.10
// critical-candidate override (measured brickwall regression).
//
// Latency: nAtt - 1 samples; delay the raw GR (and the audio in a full
// limiter) by the same amount to line up with the output.
//----------------------------------------------------------------------
lookaheadAttackReleaseSmoother(nAtt,
    nRel,
    maxAtt,
    rideOn,
    momentumOn,
    floorOn,
    flatLandOn,
    flatFullOn,
    rawGR) = hermiteAttackReleaseFollower(nB+1,
    nRel,
    rideOn,
    momentumOn,
    floorOn,
    flatLandOn*(nAtt/4),
    flatFullOn,
    cands)
    with {
        nB = int(floor(log(maxAtt)/log(2))+1);
        // the bank output is the follower's candidate list: (value,
        // deadline, npV, npD) for the attack window, then for every tap,
        // then the next-higher pair (nhV, nhD) from the release taps
        cands = rawGR:slidingMinIdxBankAtt(nAtt, maxAtt);
    };

//-------------------------------- demo ---------------------------------
// out1: delayed raw GR (the constraint the smoother must stay <= )
// out2: smoother output (rideOn = 1, momentumOn = 1, floorOn = 1:
//       the v0.4 lift-aware ride, v0.6 kink fix, v0.8 launch floor)
// out3: the v0.13 smoother (flatFullOn = 1 on top of out2's
//       flags): the schedule-aware C1 pack -- landing velocity
//       hands into the next leg, plans the schedule would outrun
//       are shortened with their velocity kept, hot flat-chord
//       entries shorten. Kills the ceiling zipper (the sawtooth
//       of deadline clamps riding a descending raw), the entry
//       clamp kink, and the v0.12 engagement kink (the launch-
//       velocity jump at every clearance failure). Same bank
// out4: the v0.9 prototype (flatLandOn = 1 on top of out2's flags):
//       within nAtt/4 samples of a pinned bottom the follower
//       commits to one flat-landing leg -- direction zero at the
//       deepest point, at the cost of bounded extra reduction below
//       the decelerating window-min on final approach. Same bank
//       instance: Faust CSE shares the cascade, its delay lines, and
//       the whole candidate scoring between all followers.
//
// Brickwall check: out2 <= out1 (likewise out3, out4) at every
// sample, up to the v0.3 hump
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
        demo(rawGR) = grPlay, smoothed, smoothedPack, smoothedFlat
            with {
                grPlay = de.delay(maxAtt-1, nAtt-1, rawGR);
                smoothed = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, 1, 1, 1, 0, 0, rawGR);
                // the v0.13 smoother: flatFullOn = 1 enables the
                // schedule-aware C1 pack (landing handoff, checked-and-
                // shortened launch, hot flat-chord shorten); differs from
                // `smoothed` ONLY in flatFullOn; the bank is shared by CSE
                smoothedPack = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, 1, 1, 1, 0, 1, rawGR);
                // the v0.9 prototype: flat landing at a pinned bottom on top
                // of out2's flags; differs from `smoothed` ONLY in flatLandOn
                smoothedFlat = lookaheadAttackReleaseSmoother(nAtt, nRel, maxAtt, 1, 1, 1, 1, 0, rawGR);
            };
    };
