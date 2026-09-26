declare name "myHermite";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026 - 2026, Bart Brouns";

// TODO: use the min(bigblock,target) as the input for the next: this works cause both the lookaheads and the prev gain are known "now"
// as long as we don't have a steady target, use the value from the bigger neigbour block.
// except the full window, which is always gliding
// the end speed of the smaller blocks use is the end speed of the bigger neigbor, as calculated by increasing putting the t of the bigger block the size of the smaller block into the future

// limit undershoot:
// before final smoother output, autosat the diff between GR in dB and fullWindow in dB, so that the max underhoot is a known dB amount 

import("stdfaust.lib");

process = //
(testSignal:hermiteLim), testSignal@look;
//
// test;

test = //
slidingMinPar(halfN, maxN, gainIsLinear)://
hermiteFB~_// "with" so we can use prev
    with {
        hermiteFB(prev) = seq(i, nBits+1, si.bus(nBits-i), hermiteOperator(i))// "with" so we can use i
            with {
                hermiteOperator(i) = min;
            };
    };

SR = 48000;
minFreq = 23.5;
gainIsLinear = 1;
maxN = 1<<int(ceil(log(SR/minFreq)/log(2)-1e-9));
look = maxN;
freq = hslider("freq[unit:Hz][scale:log]", SR/maxN, SR/maxN, SR/2, 0.01);
// we want the small window exactly half the size of the main window
halfN = int(min(maxN, max(1, ceil(SR/freq*0.5))));
n = halfN*2;

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
hermiteLim(x) = slidingMinPar(halfN, maxN, gainIsLinear, x)//
:(par(i, nBits, !), _, _)//
:hermiteFB~_
    with {
        hermiteFB(prev, halfWindow, fullWindow) = //
        // (hermiteHalf, hermiteFull):min//
        // hermite(t, p0, m0, p1, m1)//, fullWindow, t
        // (hermiteHalf, hermiteFull):min, hermiteHalf, hermiteFull
        hermiteCombined, hermiteFull, th//, hermiteHalf
            with {
                hermiteHalf = hermite(th, p0h, m0h, p1h, m1h);
                hermiteFull = hermite(t, p0, m0, p1, m1);
                hermiteCombined = select2(useHalf, hermiteFull, hermiteHalf);

                combinedWindow = min(hermiteFull, halfWindow);
                // count from 1/n to 1, so we're always gliding:
                t = (min(1, _*counting+1/n))~_;
                th = (min(1, _*countingH+2/n))~_;
                // when the target is constant, start the countdown to reach it
                counting = fullWindow==fullWindow';
                countingH = combinedWindow==combinedWindow';
                // start at the previous point and direction
                p0 = prev:ba.sAndH(sample);
                p0h = prev:ba.sAndH(sampleH);
                m0 = (prev-prev')*n:ba.sAndH(sample);
                m0h = (prev-prev')*halfN:ba.sAndH(sampleH);
                // end at the lookahead point
                p1 = fullWindow:ba.sAndH(sample);
                p1h = combinedWindow:ba.sAndH(sampleH);
                // if the m1 is not 0, it's not the end yet
                m1 = 0;
                // TODO: use the actual predicted end speed
                m1h = 0;
                // get new targets when we are not yet counting.
                sample = 1-counting;
                sampleH = 1-countingH;

                useHalf = //
                ((halfWindow<prev)//
                &(hermiteHalf<hermiteFull))//
                // |((th>(2/n))&(fullWindow>prev));
                |(countingH&(halfWindow>prev));
            };
    };
//
// nBits fixed windows that are the same size or smaller than the half window, then the half window, then the full window
//
slidingMinPar(halfN, maxN, lin) = slidingReducePar(min, halfN, maxN, lin);
slidingReducePar(op, halfN, maxN, gainIsLinear) = sequentialOperatorParOut(nBits-1, op)<:fixedWindows, (variableWindows<:halfWindow, fullWindow)
    with {
        disabledVal = gainIsLinear;
        fixedWindows = par(i, nBits, _@(look-pow2(i)):useValSize(i));
        useValSize(i) = select2(pow2(i)<=halfN, disabledVal, _);
        variableWindows = par(i, nBits, _@sumOfPrevBlockSizes(i):useValBit(i)):parallelOp(op, nBits);
        useValBit(i) = select2(isUsed(i), disabledVal, _);
        halfWindow = _@(look-halfN);
        fullWindow = (_<:op(_, _@halfN)):_@(look-2*halfN);
        sequentialOperatorParOut(HALFN, op) = seq(i, HALFN, operator(i));
        operator(i) = si.bus(i), (_<:_, op(_, _@pow2(i)));
        sumOfPrevBlockSizes(0) = 0;
        sumOfPrevBlockSizes(i) = ba.subseq(allBlockSizes, 0, i):>_;
        allBlockSizes = par(i, maxNrBits(maxN-1), pow2(i)*isUsed(i));
        isUsed(i) = ba.take(i+1, int2bin(halfN));
        parallelOp(op, 1) = _;
        parallelOp(op, HALFN) = op(parallelOp(op, HALFN-1), _);
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
