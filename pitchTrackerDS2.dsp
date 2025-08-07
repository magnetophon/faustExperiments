// TODO: smootherARorder:
// log crossfade for times
// put nonlinearitys between stages that invert each other
import("stdfaust.lib");

process(x) =
  envPitch(x)~_
              <: (attackReplacer(x) + synthLevel* CZsynth);

attackReplacer(x, env,pitch) =
  (x / max(ma.EPSILON,env))
  * attackEnv(env);

synthLevel(env, pitch) =
  (1-attackEnv(env))
  *0.6;

CZsynth(env, pitch) =
  phasesAndFilters
  : oscillators(index)
  : si.interpolate(octave:ma.frac)
with {
  fund = os.lf_sawpos(pitch*oct2mult(minOctave));
  f0 = (fund * oct2mult(floor(octave) - minOctave)):ma.frac;
  f1 = f0*2:ma.frac;
  phasesAndFilters =
    (f1, f0)
    :swap(bool)
    :(preFilter,preFilter)
    :swap(1-bool);
  swap(bool,a,b) =
    select2(bool, a, b)
  , select2(bool, b, a);
  bool =
    floor(abs(octave - minOctave))%2;
  oscillators(index, f0, f1) =
    os.CZsquare(f0,index)
  , os.CZsquare(f1,index);
  index = env+(hslider("index", 0, 0, 1, 0.001):si.smoo);
  preFilter(x) =
    (ve.korg35LPF(normFreq:max(0):min(Fthres),Q,x),x)
    :si.interpolate(BP);
  Fthres = 0.95;
  BP = (normFreq-Fthres):max(0)/(1-Fthres);

  normFreq = hslider("filt freq", 1, 0, 1, 0.001):si.smoo;
  Q = hslider("Q", 0, 0, 10, 0.001):si.smoo;
};

oct2mult(oct) = pow(2,oct);

// os.CZsquare(fund, index)

attackEnv(env) =
  (loop~_)
  * attackReplace
with {
  trig = env>threshold:ba.impulsify;
  loop(prevEnv ) =
    ( (prevEnv<1)
      & (prevEnv>0)
      & (prevEnv>prevEnv')
    )
    | trig
    :smootherARorder(4,4,2, attack, decay);
};

octave = hslider("octave", 0, minOctave, 4, 0.001): si.smoo;

maxHoldSamples = 2048;
minOctave = -4;


attack = 0.004;
// hslider("attack", 0.2, 0, 1, 0.001)*0.02;
decay = 0.15;
// hslider("decay", 0.15, 0, 0.3, 0.001);
attackReplace = 1;
// hslider("attack replace", 1, 0, 1, 0.001);

envPitch(x,prevEnv) =
  ba.slidingMax(samples(x,prevEnv),maxHoldSamples,abs(x))
  :max(0) // slidingMax defaults to -inf
  :smootherARorder(4,4,4, att, rel(x,prevEnv))
, pitch(x,prevEnv)
;

samples(x,prevEnv) =
  // hslider("mult", 1, 0.5, 2, 0.01)
  // *
  ma.SR / pitch(x,prevEnv)
  :max(0):min(maxHoldSamples)
          // :hbargraph("samples", 0, maxHoldSamples)
;

pitch(x,prevEnv) =
  pitchTracker(4,tau,x,prevEnv)
  // : hbargraph("pitch", 30.87, 500)
;

declare zcr author "Dario Sanfilippo";
declare zcr copyright "Copyright (C) 2020 Dario Sanfilippo
      <sanfilippo.dario@gmail.com>";
declare zcr license "MIT-style STK-4.3 license";
zcrN(minF,maxF,loudEnough, x) =
  select2(
    bypass(rawFreq)
    // :hbargraph("BP", 0, 1)
  , (resetAvg~_)
  ,zcRate
  )

with {
  zcRate = ma.zc(x)
           // <:select2(checkbox("dyn")
           // , smootherARorder(4,4,4, period,period)
           // ,
           : fi.dynamicSmoothing(sensitivity, baseCF)
             // )
  ;
  sensitivity = 0.07;
  // hslider("sensitivity", 0.07, 0, 1, 0.001);
  baseCF = 16;
  // hslider("base CF", 16, 6, 21, 0.001);
  rawFreq = zcRate * ma.SR * .5
            // :hbargraph("raw F", 30.87, 500)
  ;
  bypass(f) =
    loudEnough
    * (f>minF)
    * (f<maxF);
  meanSamples =
    0.2
    // hslider("mean", 0.2, 0, 0.5, 0.001)
    *ma.SR;
  // todo: only count when quality is good?
  // TODO:
  // maybe: at each reset start 2 averages: one with the current value and one with the last avg
  // or: one that resets on each pitch that is too far appart and one that only restest when the level is too low
  resetAvg(prevRate) =
    ((resetSum~_)
     / avgCounter)
    :getValAtHighetstCounter
  with {
    reset(f) =
      (1-bypass(f))
      | newNote(f)
      // : hbargraph("reset", 0, 1)
    ;
    // TODO: tweak ranges, compare f to avg
    newNote(f) =
      (f>((prevF:ba.hz2midikey+newNoteSens):ba.midikey2hz))
      | (f<((prevF:ba.hz2midikey-newNoteSens):ba.midikey2hz)) ;
    newNoteSens = 1.5;
    // hslider("new note sens", 1.5, 0, 12, 0.001);
    resetSum(prev) =
      select2(reset(rawFreq)
             , zcRate+prev
             , zcRate)
      - ((avgCounter>meanSamples)*(zcRate@meanSamples));
    prevF =
      prevRate *
      ma.SR * .5:max(minF:min(maxF));
    avgCounter = ba.countup(meanSamples, reset(rawFreq))+1
                 // :hbargraph("avg counter", 1, 0.5*48000)
    ;
    highestCounter =
      loop~_
    with {
      loop(prevHighest) =
        select2(bypass(rawFreq)
               , prevHighest
               , max( prevHighest , avgCounter)
               )
        *(1-(bypass(rawFreq):ba.impulsify))
        // :hbargraph("highest", 0, 0.5*48000)
      ;
    };
    // sample the value with the highest avgCounter value since the last bypass
    getValAtHighetstCounter(value) =
      select2(bypass(rawFreq)
             , (loop~_)
             , value
             )
    with {
      loop(prevValue) =
        select2(avgCounter>=highestCounter
               , prevValue
               , value);
    };

  };


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
  // TODO: check if this is beter:
  // xHighpassed = fi.highpass(N, minF, x);
  loop(y) =
    (zcrN(minF,maxF,prevEnv>threshold
          , fi.lowpass(N, max(minF, y), xHighpassed))
     * ma.SR * .5):max(minF):min(maxF);
};

// The lowest note on a typical 5-string bass is B0 (30.87 Hz).
minF = 30.87;
// hslider("min pitch", 30.87, 20, 100, 1);
// If the guitar has 24 frets, the highest note would be E6, which is 1318.51 Hz.
maxF =
  1318.51;
// hslider("max pitch", 420, 100, 1318.51, 1);
threshold = hslider("threshold", -22, -90, 0, 0.1):ba.db2linear;
att = 0;
rel(x,prevEnv) =
  // hslider("rel", 1, 0.1, 2, 0.01)
  1
  /pitch(x,prevEnv);

tau = hslider("tau", 42, 1, 100, 1)*0.001;

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

// correlation(x) = (x*x@(ma.SR/myPitch(x):max(0):min(maxHoldSamples)))/max(ma.EPSILON,x*x):max(0):min(1):hbargraph("correlation", 0, 1);
correlation(x) = ((x*x@(ma.SR/myPitch(x):max(0):min(maxHoldSamples)))/max(threshold,x*x)):max(0):min(1):hbargraph("correlation", 0, 1);
