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

hermiteLim(x) = slidingMinPar(n, maxN, gainIsLinear, x)//
:(par(i, nBits, !), _, _)//
:hermiteFB~_
    with {
        // hermiteFB(prevP, alignedCombined, deepWin) = hermite(t, p0, m0, p1, m1):min(x@look), alignedCombined, t
        hermiteFB(prevP, alignedCombined, deepWin) = hermite(t, p0, m0, p1, m1), alignedCombined, t
            with {
                // count from 1/n to 1, so we're always gliding:
                t = (min(1, _*counting+1/n))~_;
                // when the target is constant, start the countdown to reach it
                counting = alignedCombined==alignedCombined';
                // start at the previous point and direction
                p0 = prevP:ba.sAndH(sample);
                m0 = (prevP-prevP')*n:ba.sAndH(sample);
                p1 = alignedCombined:ba.sAndH(sample);
                // if the m1 is not 0, it's not the end yet
                m1 = 0;
                // get new targets when we are not yet counting.
                sample = 1-counting;
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
testSelect = TestGroup(checkbox("[6]signal select"));
testSignal = select2(testSelect, testSignal1, testSignal2);
testSignal1 = it.interpolate_linear(testNoiseLevel,
    (loop~_),
    no.lfnoise(testNoiseRate))
    with {
        loop(prev) = no.lfnoise0(testBlockscale*(abs(prev*69)%9:pow(0.75)*5+1));
    };
testSignal2 = os.lf_squarewave(testFreq)*0.5;
