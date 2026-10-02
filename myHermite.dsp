declare name "myHermite";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026 - 2026, Bart Brouns";

// TODO:
// on the falling edge of x (not @ look!) < fullHermite(t+?),
// or: on the rising edge of halfWindow<fullHermite(t+0.5:min(1))
//
//start a counter (+2/n), when it reaches 1, start the hermiteHalf with m1h = fullHermite(t+?)-fullHermite(t+?-1)
//
//
//use the min(bigblock,target) as the input for the next: this works cause both the lookaheads and the prev gain are known "now"
// as long as we don't have a steady target, use the value from the bigger neigbour block.
// except the full window, which is always gliding
// the end speed of the smaller blocks use is the end speed of the bigger neigbor, as calculated by increasing putting the t of the bigger block the size of the smaller block into the future

// limit undershoot:
// before final smoother output, autosat the diff between GR in dB and fullWindow in dB, so that the max underhoot is a known dB amount 

// shaper on t
// and/or: shaper+antishaper on GR
//
// fix stuck release at the bottom:
// if
// speed < possiblespeed & t = 1/n
// ramp up to possiblespeed
//
//something similar shoukd be done for stuck attacks that are going down too much: unstick them
//
// when coasting, cheack how far we actually are vs how far t (or th) is, if it is not far, use the ramp
// also check if we are going dangerously close to the p1 of the main hermite, for example:
// when releaseing, check if the new taret is lower than the old one
//
// only grigger useHalf when x@? islower than fullHermite at t in the future
//
//
//  during coasting, assume the target will go in a straight line for n samples
//  mult phantom by distance between
//  prev-target
//
// only one *sample in p1 calc: at the (almost) end.
//
// if halfWindow<prev: n=n/2

import("stdfaust.lib");

process = //
testSignal@look, (testSignal:hermiteLim);
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
// minFreq = 12000;
// minFreq = 11999;
gainIsLinear = 1;
maxN = (1<<int(ceil(log(SR/minFreq)/log(2)-1e-9)));
// allign the signal properly
look = maxN-1;
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
        hermiteFull, p1, kc*close*(ceiling-smootherPrediction), abs(second)*10, th*useHalf

        //t, (abs(prev-prev')*100, _:max)~_, t, th, hermiteFull, hermiteHalf//th , hermiteHalf
            with {
                //////////////////// debug
                first = prev-prev';
                second = first-first';
                // note: has clamp at 1!
                maxHold(x) = max(min(1, abs(x)), _)~_;
                // note: has abs!
                max2nd = maxHold(abs(second));
                //////////////////// debug

                err = fullWindow-prev;
                vn = (prev-prev')*n;
                // velocity per window, same units as err
                ceiling = x@look;
                close = sample*(1-max(0, ceiling-prev))^power;
                // close = (ceiling-prev)/(fullWindow-prev);
                // close = sample*((smootherPrediction-ceiling)*boost);
                power = hslider("power", 32, 1, 128, 1);
                base = hslider("base", 0.5, 0, 1, 0.001);
                // 1 at the ceiling, 0 at distance s
                urgency = base+kc*close;
                push = (kp*err-kd*vn)*urgency;

                phantomTarget = (fullWindow+push*sample):min(phantomTarget_old);
                //:max(-1):min(fullWindow);

                kp = hslider("kp", 1, 0, 2, 0.001);
                // overshoot per unit of error
                kd = hslider("kd", 1, 0, 2, 0.001);
                // damping / turn-around strength
                kc = hslider("kc", 13, 0, 100, 0.01);
                // extra urgency near the ceiling
                s = hslider("s[scale:log]", 0.1, 0.01, 2, 0.001);
                // "close" range

                boost = hslider("boost", 1, 0, 13, 0.001);

                // inside hermiteFB's `with`
                // e = fullWindow-prev;
                // dirE = select2(e>=0, -1, 1);
                // toward = (prev-prev')*n*dirE;
                // speed toward the target, in units per window
                // shortfall = (abs(e)-toward):max(0);
                //:min(kmax*abs(e));

                // phantomTarget = (fullWindow+dirE*mult*shortfall*sample);

                // kmax = hslider("kmax", 2, 1, 8, 0.01);
                // cap on the push, in multiples of |e|
                // how far the gain still has to fall (0 when not attacking)
                err_old = max(0, p0-fullWindow);
                // boost = hslider("boost", 1, 0, 10, 0.001);
                maxOver = hslider("max overshoot", 0.5, 0, 2, 0.001);

                windowPredictionRaw = n*(fullWindow-fullWindow');

                smallestAbs(x) = select2(abs(x)<abs(x'), x', x);

                windowPrediction = smallestAbs(windowPredictionRaw)+fullWindow;

                smootherPrediction = prev+vn;

                phantomTargetRaw = fullWindow+(sample*n*(fullWindow-fullWindow')*mult*(1+err_old*boost));
                //
                // :max(fullWindow-err_old*maxOver)// never overshoot more than a fraction of the distance
                // :max(-1):min(1);
                // phantomTarget = (phantomTargetRaw+phantomTargetRaw')*0.5:max(-1):min(1);
                phantomTarget_old = select2(abs(phantomTargetRaw)>abs(phantomTargetRaw'),
                    phantomTargetRaw,
                    phantomTargetRaw'):max(-1):min(fullWindow);
                phantomTargetx = select2(prev<fullWindow,
                    max(phantomTargetRaw, phantomTargetRaw'),
                    min(phantomTargetRaw, phantomTargetRaw'));

                // hermiteHalf = hermite(th, p0h, m0h, p1h, m1h):cascade(freq*2*mult, sampleH, prev, halfWindow);
                hermiteHalf = hermite(th, p0h, m0h, p1h, m1h);
                // hermiteFull = hermite(t, p0, m0, p1, m1):cascade(freq*mult, sample, fullWindow);
                hermiteFull = hermite(t, p0, m0, p1, m1);
                hermiteCombined = select2(useHalf, hermiteFull, hermiteHalf);
                //:cascade(freq*mult, sample, fullWindow);

                mult = hslider("mult", 1, 0, 42, 0.001);

                combinedWindow = min(hermiteFull, halfWindow);
                // count from 1/n to 1, so we're always gliding:
                t = (min(1, _*counting+1/n))~_;
                WIP_t = tFB~_
                    with {
                        tFB(prevT) = (min(1, prevT*counting*(1-(isFast(prevT):ba.impulsify))+(1+isFast(prevT))/n));
                    };

                isFast(prevT) = halfWindow<hermite(min(1, prevT+halfN), p0, m0, fullWindow, m1);
                th = (min(1, _*countingH+2/n))~_;
                tStuck = (min(1, _*sample+1/n))~_;
                // when the target is constant, start the countdown to reach it
                counting = fullWindow==fullWindow';
                countingH = combinedWindow==combinedWindow'|counting;
                // start at the previous point and direction
                p0 = prev:ba.sAndH(sample);
                p0h = prev:ba.sAndH(sampleH);
                m0 = (prev-prev')*n:ba.sAndH(sample);
                m0h = (prev-prev')*halfN:ba.sAndH(sampleH);
                // end at the lookahead point
                // p1 = sample*mult*(windowPrediction-smootherPrediction)+fullWindow;
                p1 = (sample*base*(fullWindow-smootherPrediction)+kc*close*(ceiling-smootherPrediction))+fullWindow;
                p1_old = select2(counting,
                    phantomTarget,
                    fullWindow:ba.sAndH(sample));
                p1h = combinedWindow:ba.sAndH(sampleH);
                // if the m1 is not 0, it's not the end yet
                m1 = sample*(fullWindow-fullWindow);
                // TODO: use the actual predicted end speed
                // use one more sample latency:
                // these are the old look values, so signal needs to be delayed by (look+1)
                // dirPast = x*look-x@(look-1);
                // dirFuture = x@(look+1)-x*look;
                // m1 = select2(dirPast)
                m1h = 0;
                // get new targets when we are not yet counting.
                sample = 1-counting;
                sampleH = 1-countingH;

                // TODO: fix discontinuity
                useHalf = //
                useHalfFB~_
                    with {
                        useHalfFB(prevUseHalf) = //
                        // ((p0>p1)|(p0h>p1h))//
                        ((p0h>p1h)// are we attacking?
                        |((p0>p1)&prevUseHalf)// the long window can only keep useHalf on, not trigger it
                        )//
                        &(hermiteHalf<hermiteFull);
                    };
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
        halfWindow = _@(maxN-halfN);
        fullWindow = (_<:op(_, _@halfN)):_@(maxN-2*halfN);
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
testNoiseLevel = TestGroup(hslider("[0]noise level", 0.216, 0, 1, 0.001));
testNoiseRate = TestGroup(hslider("[1]noise rate[scale:log]", 42, 1, 1000, 1));
testBlockscale = TestGroup(hslider("[2]blockscale[scale:log]", 6.63, 0.01, 10, 0.01));
testFreq = TestGroup(hslider("[3]test freq[scale:log]", 1, 0.001, 30, 0.001));
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

/************************************************************************************************************
**************         utilities
************************************************************************************************************/

// minDelta(0) = 0.001;
minDelta(0) = hslider("minDelta", 0.001, 0, 1, 0.001)*0.001;
minDelta(1) = ba.db2linear(minDelta(0));
curvesMerged = abs(hermiteHalf-hermiteFull)<minDelta(gainIsLinear);

// f: cutoff of each pole in Hz. v0 is in units per sample.
// On a rising edge of `reset`, the states are loaded so the output starts at p0 heading with slope v0.
cascade(f, set, target, p0) = target:stage(p0+v0/a):stage(p0)
    with {
        v0 = p0-p0';
        a = 1-exp(-2*ma.PI*f/ma.SR);
        stage(s0, x) = (step~_)
            with {
                step(y1) = select2(set, s0, y1+a*(x-y1));
            };
    };

// f: natural frequency in Hz (higher = snappier), zeta: 1 = critically damped,
// <1 = overshoot, >1 = sluggish.
// On a rising edge of `reset`, jump to (p0, v0): "start at a point, heading this way".
// v0 is in units per sample.
spring(f, zeta, reset, p0, v0, target) = (step~(_, _)):(_, !)
    with {
        w = 2*ma.PI*f/ma.SR;
        k = w*w;
        c = 2*zeta*w;
        trig = reset>reset';
        step(y1, v1) = y, v
            with {
                vFree = v1+k*(target-y1)-c*v1;
                v = select2(trig, vFree, v0);
                y = select2(trig, y1+vFree, p0);
            };
    };

// process = spring(2, 1, button("start"), 0, 0.001, os.osc(0.5));
