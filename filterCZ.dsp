declare name "filtered CZ";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2025 - 2025, Bart Brouns";




// TODO: smootherARorder:
// log crossfade for times
// put nonlinearitys between stages that invert each other
import("stdfaust.lib");
import("pitchTracker.lib");



thresholdSlider = hslider("h:/v:main/[0]threshold", -22, -44, 0, 0.1):ba.db2linear;
levelMod = hslider("h:/v:mod/[1]level mod", 1, 0, 2, 0.001):si.smoo;
octaveSlider = hslider("h:/v:main/[2]octave", 0, minOctave, 4, 0.001): si.smoo;
octMod = hslider("h:/v:mod/[2]octave mod", 0, -11, 11, 0.001): si.smoo;
indexSlider = hslider("h:/v:main/[3]index", 0, 0, 1, 0.001):si.smoo;
indexMod = hslider("h:/v:mod/[3]index mod", 0, -4, 4, 0.001):si.smoo;
filterFreqSlider = hslider("h:/v:main/[4]filt freq", 1, 0, 1, 0.001):si.smoo;
filterFreqMod = hslider("h:/v:mod/[4]filt freq mod", 0, -4, 4, 0.001):si.smoo;
Qslider = hslider("h:/v:main/[5]Q", 0, 0, 10, 0.001):si.smoo;
Qmod = hslider("h:/v:mod/[5]Q mod", 0, -30, 30, 0.001):si.smoo;


minOctave = -4;


process(x) =
  envPitch(thresholdSlider,x)~_
                              <: (attackReplacer(x) + synthLevel* CZsynth)*totalLevel;

attackReplacer(x, env,pitch) =
  (x / max(ma.EPSILON,env))
  * attackEnv(env);

synthLevel(env, pitch) =
  (1-attackEnv(env))
  *0.6;

// remap(from1, from2, to1, to2, x)

xfadeSelectorOLD(sel,nr) =
  ((sel<=nr)*((sel-nr)+1):max(0)) + ((sel>nr)*((nr-sel)+1):max(0));

xfadeSelector(sel, nr) = (1 - abs(sel - nr)) : max(0);

totalLevel(env, pitch) =
  (env/0.6)*xfadeSelector(levelMod, 0)
  + blockLevel(env)*xfadeSelector(levelMod, 1)
  + xfadeSelector(levelMod, 2)
;

blockLevel(env) =
  it.remap(thresholdSlider*0.5,thresholdSlider, 0, 1,env:max(thresholdSlider*0.5):min(thresholdSlider));

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
  preFilter(x) =
    (ve.korg35LPF(filterFreq:max(0):min(Fthres),Q,x),x)
    :si.interpolate(filterBP);
  Fthres = 0.95;
  filterBP = (filterFreq-Fthres):max(0)/(1-Fthres);

  octave = (octaveSlider + octMod*env):max(minOctave);
  index = indexSlider+ indexMod*env;
  filterFreq = filterFreqSlider + filterFreqMod*env:max(0):min(1);
  Q = Qslider + Qmod*env:max(0):min(10);

};

oct2mult(oct) = pow(2,oct);


attackEnv(env) =
  (loop~_)
  * attackReplace
with {
  trig = env>thresholdSlider:ba.impulsify;
  loop(prevEnv ) =
    ( (prevEnv<1)
      & (prevEnv>0)
      & (prevEnv>prevEnv')
    )
    | trig
    :smootherARorder(4,4,2, attack, decay);
};




attack = 0.004;
// hslider("attack", 0.2, 0, 1, 0.001)*0.02;
decay = 0.15;
// hslider("decay", 0.15, 0, 0.3, 0.001);
attackReplace = 1;
// hslider("attack replace", 1, 0, 1, 0.001);

