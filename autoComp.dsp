import("stdfaust.lib");

OnePoleTPT = environment {
    OP(CF, x) = f~_:(!, _, _, _)
        with {
            g = tan(CF*ma.PI*ma.T);
            G = g/(1.0+g);
            f(s) = u, LP, HP, AP
                with {
                    v = (x-s)*G;
                    // zero-delay-feedback solve
                    LP = s+v;
                    // lowpass output
                    HP = x-LP;
                    // highpass (free)
                    AP = LP-HP;
                    // first-order allpass (free)
                    u = LP+v;
                    // new integrator state
                };
        };
    LP1(CF, x) = OP(CF, x):ba.selectn(3, 0);
    HP1(CF, x) = OP(CF, x):ba.selectn(3, 1);
    AP1(CF, x) = OP(CF, x):ba.selectn(3, 2);
};

// two att-rel filters: one main and one ref.
// the distance between the two determines the cutoff of each filter:
// when the distance is big, the main att gets slower and release faster and the ref att gets faster and the ref release slower

gain_computer(strength, thresh, knee, level) = select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
    0,
    ((level-thresh+(knee/2)):pow(2)/(2*max(ma.EPSILON, knee))),
    (level-thresh)):max(0)*-strength;

// autoAttRel(x) = loop~(_, _)
// with {
// loop(prevgain,prevRef)
// gain_computer(strength, thresh, knee, level)};

// TODO: put smoothing after channel-link in N-chan version
compression_gain_mono_db_auto(strength, thresh, knee, level) = loop~(_, _):(_, !)
    with {
        loop(prevGain, prevRef) = gain, ref
            with {
                gain = gain_computer(1, thresh, knee, level)*strength:releaseHold:si.onePoleSwitching(fastTime, 0):hbargraph("[0]gain[unit:dB]", -24, 0);

                // used for both the release of gain and the attack of ref
                fastTime = interpolate_logarithmic(dv, mediumTime, 1/6000);

                mediumTime = hslider("mediumTime[scale:log]", 0.42, 0.001, 1, 0.001);
                longRel = hslider("longRel[scale:log]", 13, 1, 100000000, 0.1);

                ref = (prevGain-transitionRange):min(0)*strength:si.onePoleSwitching(refRel, fastTime):hbargraph("[1]ref[unit:dB]", -24, 0);
                // :ba.db2linear// : smootherOrder(maxOrder,refOrder,refRel,0)
                // :smootherOrder(1, 1, refRel, 0):ba.linear2db// : hbargraph("ref[unit:dB]", -24, 0) ;
                refRel = interpolate_logarithmic(dv,
                    mediumTime,
                    longRel);
                dv = (fastGR/transitionRange):max(0):min(1):hbargraph("dv", 0, 1);
                // dv = (fastGR:min(0)/transitionRange):min(1):hbargraph("dv", 0, 1);
                fastGR = (prevGain-prevRef);
                // :hbargraph("fast GR[unit:dB]", -24, 0);
            };
    };

interpolate_logarithmic(dv, v0, v1) = v0*pow(v1/v0, dv);
transitionRange = hslider("transitionRange", -6.9, -30, 0.1, 0.1);

compressor(l, r) = l@rel_hold_samples*gain, r@rel_hold_samples*gain, gain
    with {
        gain = compression_gain_mono_db_auto(1, thres, 0, max(abs(l), abs(r)):ba.linear2db):ba.db2linear;
    };

thres = hslider("thres[unit:dB]", -1, -30, 0, 0.1);

// ============================================================================
//  RELEASE HOLD (from lamb)
// ============================================================================
maxRelHold = 0.05;
maxRelHoldSamples = maxRelHold*maxSR;
maxSR = 192000;

relHoldMs = hslider("[0]rel hold[unit:ms][scale:log]", 50, 0.1, maxRelHold*1000, 0.1);
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

process = compressor;
