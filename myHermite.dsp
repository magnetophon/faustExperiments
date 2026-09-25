declare name "myHermite";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026 - 2026, Bart Brouns";

// TODO: use the min(bigblock,target) as the input for the next: this works cause both the lookaheads and the prev gain are known "now"
// AB ramps: retrigger:impulsify

import("stdfaust.lib");

process = (testSignal:hermiteLim), testSignal@look;

SR = 48000;
minFreq = 23.5;
gainIsLinear = 1;
maxN = 1<<int(ceil(log(SR/minFreq)/log(2)-1e-9));
look = 2*maxN;
freq = hslider("freq[unit:Hz][scale:log]", SR/maxN, SR/maxN, SR/2, 0.01);
n = int(min(maxN, max(1, ceil(SR/freq))));

nBits = maxNrBits(maxN);
maxNrBits(x) = int(floor(log(x)/log(2))+1);
// hermite interpolator
//
// t=time {0<t<1}
// p0 = startpoint
// m0 = startdirection
// p1 = endpoint
// m1 = enddirection
// https://www.desmos.com/calculator/2mpyoyno2j
hermite(t, p0, m0, p1, m1) = ((a*t+b)*t+m0)*t+p0
    with {
        a = 2*p0+m0-2*p1+m1;
        b = -3*p0-2*m0+3*p1-m1;
    };
// Fritsch–Carlson monotone tangent limiter: rescales m0,m1 so the
// resulting cubic Hermite segment can't overshoot p0..p1.
// https://en.wikipedia.org/wiki/Monotone_cubic_interpolation
monotoneTangents(p0, p1, m0, m1) = p0, m0f, p1, m1f
    with {
        delta = p1-p0;
        flat = (delta==0);
        deltaSafe = delta+flat;
        // avoid /0; result unused when flat
        alpha = select2(flat, max(0, m0/deltaSafe), 0);
        beta = select2(flat, max(0, m1/deltaSafe), 0);
        r2 = alpha*alpha+beta*beta;
        tau = select2(r2>9, 1, 3/sqrt(max(1e-20, r2)));
        m0f = tau*alpha*delta;
        m1f = tau*beta*delta;
    };

hermiteLim(x) = slidingMinPar(n, maxN, gainIsLinear, x)//
:(par(i, nBits, !), _, _)//
:hermiteFB~_
    with {
        // hermiteFB(prevP, alignedCombined, deepWin) = hermite(t, p0, m0, p1, m1):min(x@look), sample, t
        hermiteFB(prevP, alignedCombined, deepWin) = hermite(t, p0, m0, p1, m1), alignedCombined, t
            with {
                t = (min(1, _+1/n)*counting)~_;
                // try to stay at least 1/n cheap:
                // t = (min(1, _*counting*(1-targetReached)+1/n))~_;
                // counting_new = alignedCombined==alignedCombined'*(1-targetReached);
                // counting = alignedCombined==alignedCombined'*(1-(targetReached:ba.impulsify));
                counting = alignedCombined==alignedCombined';
                attacking = alignedCombined<prevP;
                releasing = alignedCombined>prevP;
                p0 = prevP:ba.sAndH(sample);
                m0 = (prevP-prevP')*n:ba.sAndH(sample);
                p1 = alignedCombined:ba.sAndH(sample);
                // m1 = (alignedCombined-deepWin)/n<:select2(attacking, max(0), min(0)):ba.sAndH(sample);
                // if the m1 is not 0, it's not the end yet
                m1 = 0;
                //(deepWin-alignedCombined):ba.sAndH(sample);
                sample = t<(1/n);
                minDelta(0) = 0.000001// dB
                ;
                minDelta(1) = 1-ba.db2linear(0-minDelta(0));
                targetReached = abs(prevP-x@look)<minDelta(gainIsLinear);
            };
    };

slidingMinPar(n, maxN, lin) = slidingReducePar(min, n, maxN, lin);
slidingReducePar(op, n, maxN, gainIsLinear) = sequentialOperatorParOut(nBits-1, op)<:parTaps, (combined<:alignedCombined, deepWin)
    with {
        look = 2*maxN;
        disabledVal = gainIsLinear;
        parTaps = par(i, nBits, _@(look-pow2(i)):useValSize(i));
        useValSize(i) = select2(pow2(i)<=n, disabledVal, _);
        combined = par(i, nBits, _@sumOfPrevBlockSizes(i):useValBit(i)):parallelOp(op, nBits);
        useValBit(i) = select2(isUsed(i), disabledVal, _);
        alignedCombined = _@(look-n);
        deepWin = (_<:op(_, _@n)):_@(look-2*n);
        sequentialOperatorParOut(N, op) = seq(i, N, operator(i));
        operator(i) = si.bus(i), (_<:_, op(_, _@pow2(i)));
        sumOfPrevBlockSizes(0) = 0;
        sumOfPrevBlockSizes(i) = ba.subseq(allBlockSizes, 0, i):>_;
        allBlockSizes = par(i, maxNrBits(maxN-1), pow2(i)*isUsed(i));
        isUsed(i) = ba.take(i+1, int2bin(n));
        parallelOp(op, 1) = _;
        parallelOp(op, N) = op(parallelOp(op, N-1), _);
        int2bin(x) = par(j, nBits, int(floor(x/pow2(j)))%2);
        pow2(i) = 1<<i;
    };

/************************************************************************************************************
**************         testSignal
************************************************************************************************************/

MainGroup(x) = hgroup("[0]Main", x);
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
