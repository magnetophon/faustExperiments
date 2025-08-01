import("stdfaust.lib");
process(x) =
  x
, (
  0.25
  // envelope(x)
  *os.osc(myPitch(x)*2))
  // , ba.slidingMax(samples(x,1),maxHoldSamples,abs(x))
,
  envelope(x)
;


// maxHoldSamples = 2048;
maxHoldSamples = 4096;

envelope(x) =
  envLoop(x)~_:(_,!);
myPitch(x) =
  envLoop(x)~_:(!,_);
// (ba.slidingMax(samples(x,_),maxHoldSamples,abs(x))
//  :max(0) // slidingMax defaults to -inf
//  :smootherARorder(4,4,4, att, rel(x,_)))~(_<:(_,_));

envLoop(x,prevEnv) =
  ba.slidingMax(samples(x,prevEnv),maxHoldSamples,abs(x))
  :max(0) // slidingMax defaults to -inf
  :smootherARorder(4,4,4, att, rel(x,prevEnv))
, pitch(x,prevEnv)
;

samples(x,prevEnv) =
  hslider("mult", 1, 0.5, 2, 0.01)
  *ma.SR / pitch(x,prevEnv)
  :max(0):min(maxHoldSamples)
  :hbargraph("samples", 0, maxHoldSamples);

pitch(x,prevEnv) =
  pitchTracker(4,tau,x,prevEnv)
  // :max(30.87)
  // :min(maxF)
  : hbargraph("pitch", 30.87, 500)
;

// * `tau`: (time to decay by e^-1) sets the averaging frame in seconds.
declare zcr author "Dario Sanfilippo";
declare zcr copyright "Copyright (C) 2020 Dario Sanfilippo
      <sanfilippo.dario@gmail.com>";
declare zcr license "MIT-style STK-4.3 license";
// zcrN(N,minF,maxF,loudEnough,period, prevF, x) =
zcrN(N,minF,maxF,loudEnough,period, x) =
  select2(
    bypass(rawFreq):hbargraph("BP", 0, 1)
  , ba.slidingMean(meanSamples,zcRate)
    @(
      meanSamples
      // hslider("back mult", 4, 0, 16, 1)* ma.SR / rawFreq
      // :max(0):min(maxHoldSamples)
    )
    :ba.sAndH(bypass(rawFreq))
     // ,(zcRate@(
     // hslider("back mult", 4, 0, 16, 1)* ma.SR / rawFreq
     // :max(0):min(maxHoldSamples)):ba.sAndH(bypass(rawFreq)))
  ,zcRate
  )
with {
  zcRate = ma.zc(x)
           : smootherARorder(8,N,N, period,period);
  rawFreq = zcRate * ma.SR * .5:hbargraph("raw F", 30.87, 500);
  // TODO: when we get too quiet:
  // - hold both the current pitch and the (avg?) pitch from one cylce back
  // - crossfade during 1 cycle between the current hold and the older one
  bypass(f) =
    loudEnough
    * (f>minF)
    * (f<maxF)
  ;
  meanSamples = hslider("mean", 0.1, 0, 0.5, 0.001)*ma.SR;
};



//==============================Adaptive Frequency Analysis===============================
//========================================================================================

//--------------------`(an.)pitchTracker`---------------------------------------
//
// This function implements a pitch-tracking algorithm by means of
// zero-crossing rate analysis and adaptive low-pass filtering. The design
// is based on the algorithm described in [this tutorial (section 2.2)](https://github.com/grame-cncm/faust/blob/master-dev/documentation/misc/Faust_tutorial2.pdf).
//
// #### Usage
//
// ```
// _ : pitchTracker(N, tau) : _
// ```
//
// Where:
//
// * `N`: a constant numerical expression, sets the order of the low-pass filter, which
//  determines the sensitivity of the algorithm for signals where partials are
//  stronger than the fundamental frequency.
// * `tau`: response time in seconds based on exponentially-weighted averaging with tau time-constant. See <https://ccrma.stanford.edu/~jos/st/Exponentials.html>.
declare pitchTracker author "Dario Sanfilippo";
declare pitchTracker copyright "Copyright (C) 2022 Dario Sanfilippo
      <sanfilippo.dario@gmail.com>";
declare pitchTracker license "MIT License";
pitchTracker(N, t, x,prevEnv) = loop ~ _
with {
  xHighpassed = fi.highpass(1, 20.0, x);
  loop(y) = (zcrN(hslider("zcr N" , 1, 1, 8, 1)
                 , minF,maxF,prevEnv>threshold
                 ,t, fi.lowpass(N, max(20.0, y), xHighpassed)) * ma.SR * .5):max(minF:min(maxF));
  // ,t, fi.lowpass(N, max(20.0, y), xHighpassed)) * ma.SR * .5)~_;
};

minF = hslider("min pitch", 20, 20, 100, 1);
maxF = hslider("max pitch", 200, 100, 500, 1);
threshold = hslider("threshold", -30, -90, 0, 0.1):ba.db2linear;
att = 0;
rel(x,prevEnv) = hslider("rel", 1, 0.1, 2, 0.01)/pitch(x,prevEnv);

tau = hslider("tau", 42, 0.1, 100, 0.01)*0.001;

smootherARorder(maxOrder,orderAtt, orderRel, att, rel, xx) =
  xx : seq(i, maxOrder, loop(i) ~ _)
with {
  loop(i,fb, x) = coeff(i) * fb + (1.0 - coeff(i)) * x
  with {
  cutoffCorrection(order) = 1.0 / sqrt(pow(2.0, 1.0 / order) - 1.0);
  coeff(i) =
    ba.if(x > fb, attCoeff(i), relCoeff(i) );
  attCoeff(i) =
    exp(-TWOPIT * cutoffCorrection(orderAtt) / max(ma.EPSILON, att))
    * (i<orderAtt);
  relCoeff(i) =
    exp(-TWOPIT * cutoffCorrection(orderRel) / max(ma.EPSILON, rel))
    * (i<orderRel);
  TWOPIT = 2 * ma.PI * ma.T;
};
};
