declare options "[midi:on]";
declare options "[midi:on][nvoices:1]";
import("stdfaust.lib");

osc_group(i,x) = hgroup("osc %i", x);
// process = os.osc(freq)*gate;
// effect = dm.zita_light;
process =
  // level,gate,gate

  // subOsc(targetFreq,freq,0,0,gate)
  // ;
  // SubSynth(freq,gain,gate);
  SubSynthN(4,freq,gain,gate);

// os.osc(freq:max(440))*mono_gate*gain;
// os.osc(freq:max(440))*mono_gate;

// (0.1+mono_tst)*os.osc(440);
// par(i, Nr,
// lf_sawpos_phase_reset(440+(detune/(i+1)),0.5,button("reset"))
// ):>_/Nr with {
// Nr = 400;
// detune = hslider("detune", 0, 0, 88, 0.1);
// };


lf_sawpos_reset(freq,reset) = ma.frac * dontReset ~ +(freq/ma.SR)
with {
  dontReset  = reset:ba.impulsify*-1+1;
};
lf_sawpos_phase_reset1(freq,phase,reset) = lf_sawpos_reset(freq,reset) +phase :ma.frac; // 400 take 60% CPU


lf_sawpos_phase_reset(freq,phase,reset) = (+(phase-phase') : ma.frac *dontReset) ~ +(freq/ma.SR)
with {
  dontReset  = reset:ba.impulsify*-1+1;
};

// os.osc(440)*hslider("velocity_on 2 [midi:keyon 2]", 0, 0, 127, 1);
// os.osc(440)*gate;
// os.osc(440)<:(_*table(1:int),_*table(one:int)) with {
// table(read) = rwtable(1, 0.0,1,val ,read);
// one = hslider("one", 1, 1, 1, 1);
// val = hslider("val", 0.5, 0, 1, 0.1);
// };

// os.osc(440)*setReset(on,off);
// os.osc(440)*rwtable(6, 0.0,write,val ,read);
// setReset(on,off);
read = hslider("read", 1, 1, 1, 1):int;
// read = hslider("read", 0, 0, 5, 1):int;
write = hslider("write", 0, 0, 5, 1):int;
val = hslider("val", 0.5, 0, 1, 0.1);

// on= hslider("velocity_on  2 [midi:keyon  2]", 0, 0, 127, 1):ba.impulsify;
// off= hslider("velocity_off  2 [midi:keyoff  2]", 0, 0, 127, 1):ba.impulsify;
on= gate>gate';
off= gate<gate';

setReset(set,reset) =
  // (_|set)~(_*(reset*-1+1));
  // rwtable(8, 0.0,set,1,read);
  // rwtable(8, 0.0,set|reset:int,set * (reset*-1+1) ,1:int);
  rwtable(6, 0.0, (set|reset)*2, set * (reset*-1+1) ,read*2);
// rwtable(6, 0.0,write,val ,read);
set = button("set");
reset = button("reset");
// route(8,2,3,1,5,2,8,1);

// gain = hslider("gain [midi:ctrl 7]",1,0,1,0.001);
gain = hslider("gain",0.5,0,1,0.01);
f = hslider("freq",392.0,minFreq,maxFreq,0.001);
b = ba.semi2ratio(hslider("bend [midi:pitchwheel]",0,-2,2,0.001));
gate = button("gate");

freq = f*b;

mono_pitch = hslider("/h:midi/pitch", 64, 32, 100, 1);

mono_gain = hslider("/h:midi/gain", 1, 0, 1, 0.01);

// mono_gate = button("/h:midi/gate");
i=1;
vel_table(write,x,read)  = rwtable(129, 0.0,write,x,read);
time_table(write,x,read) = rwtable(129, 0.0,write,x,read);
// mono_gain =
// mono_gate = par(i, 127, vel_table(128,0,i)):>_>0;
mono_tst = par(i, 127, vel_table(writeIndexVel(i),vel(i),read*i)) :>_>0 with {
  vel(i) = velocity_on(i) *on(i) *(off(i)*-1+1);
  on(i)  = velocity_on(i) :ba.impulsify;
  // on(i) = gate>gate';
  off(i) = velocity_off(i):ba.impulsify;
  // off(i) = gate<gate';
  writeIndexTime(i) = select2(on(i)        , 128, i):int;
  writeIndexVel(i)  = select2(on(i) | off(i) , 128, i):int;
  velocity_on(k) = hslider("velocity_on  %k [midi:keyon  %k]", 0, 0, 127, 1);
  velocity_off(k)= hslider("velocity_off %k [midi:keyoff %k]", 0, 0, 127, 1);
};
mono_gate = par(i, 127, vel_table(writeIndexVel(i),vel(i),i)) :>_>0 with {
  vel(i) = velocity_on(i) *on(i) *(off(i)*-1+1);
  on(i)  = velocity_on(i) :ba.impulsify;
  off(i) = velocity_off(i):ba.impulsify;
  writeIndexTime(i) = select2(on(i)        , 128, i):int;
  writeIndexVel(i)  = select2(on(i) | off(i) , 128, i):int;
  velocity_on(k) = hslider("velocity_on  %k [midi:keyon  %k]", 0, 0, 127, 1);
  velocity_off(k)= hslider("velocity_off %k [midi:keyoff %k]", 0, 0, 127, 1);
};


subOsc(target,freq,absFreqOffset,retrigger,gate) =
  slaveSine(fund)
 ,slaveSine(fund*2:ma.frac)
  : xfade(fade)
// ,fund
// ,gate
with {
  // masterOsc(freq,trig) = lf_sawpos_trig((freq+absFreqOffset)/octMult(baseOct),trig) with {
  masterOsc(freq,trig) = lf_sawpos((freq+absFreqOffset)/octMult(baseOct)) with {
  // todo: PR for removing the ' (mem) after freq, check the rest
  lf_sawpos(freq) = ma.frac ~ +(freq/ma.SR);
  saw1(freq) = 2.0 * lf_sawpos(freq) - 1.0;
  lf_saw(freq) = saw1(freq);
  lf_sawpos_trig(freq,trig) = ma.frac * dontReset ~ +(freq/ma.SR)
  with {
    dontReset  = trig:ba.impulsify*-1+1;
  };

  saw1_trig(freq,trig) = 2.0 * lf_sawpos(freq,trig) - 1.0;
  lf_saw_trig(freq,trig) = saw1(freq,trig);

  };

  oct =
    octMult(
      ( ma.log2
        ((target/freq)*octMult(baseOct))
        :floor
      )
    );
  fade =   (target/((freq*oct)/octMult(baseOct)))
           :ma.frac;
  meter = _<:(_, (max(0):min(1):hbargraph("ramp", 0, 1))):attach;
  octMult(octave) = pow2(octave);
  // octMult(octave) = pow(2,octave);
  fund = masterOsc(freq,gate*retrigger)*(oct):ma.frac;
  slaveSine(fund) = fund*ma.PI*2:sin;
};

SubSynth(freq,gain,gate) =
  // freq+(gate:ba.impulsify:si.lag_ud(0,decay)*punch )<:(
  (freq,(gate:ba.impulsify:si.lag_ud(0,decayF)*punchF ), retrigger, gate  ) <: //ro.interleave(2,4):
  (
    osc_group(1,subOsc(30 +(gate:ba.impulsify:si.lag_ud(0,decayT)*punchT ) )*ba.db2linear(-6))
  , osc_group(2,subOsc(42 +(gate:ba.impulsify:si.lag_ud(0,decayT)*punchT ) )*ba.db2linear(0))
  , osc_group(3,subOsc(84 +(gate:ba.impulsify:si.lag_ud(0,decayT)*punchT ) )*ba.db2linear(-12))
  , osc_group(4,subOsc(102+(gate:ba.impulsify:si.lag_ud(0,decayT)*punchT ) )*ba.db2linear(-18))
  ):>_*gainEnvelope/4<:(_,_)
with {
  gainEnvelope = en.smoothEnvelope(0.01,gate)*gain;
};

retrigger = checkbox("retrigger");
target_group(x) = hgroup("target", x);
freq_group(x) = hgroup("freq", x);

punchT = target_group(hslider("[1]punch T", 0, 0, 3000, 1));
decayT = target_group(hslider("[2]decay T", 0.02, 0, 0.1, 0.001));
punchF = freq_group(hslider("[1]punch F", 0, 0, 3000, 1));
decayF = freq_group(hslider("[2]decay F", 0.02, 0, 0.1, 0.001));

SubSynthN(N,freq,gain,gate) =
  (freq,(gate:ba.impulsify:si.lag_ud(0,decayF)*punchF ), retrigger, gate  ) <: //ro.interleave(2,4):
  (
    // si.bus(4)
    // par(i, N, osc_group(i,subOsc(targetFreq +(gate:ba.impulsify:si.lag_ud(0,decayT)*punchT ) )*ba.db2linear(level)))
    par(i, N, osc_group(i,subOsc(targetFreq +(gate:ba.impulsify:si.lag_ud(0,decayT)*punchT ) )*level))
      // par(i, N, osc_group(i,subOsc(targetFreq)*level))
  ):>_*gainEnvelope/N<:(_,_)
 ,gainEnvelope
// ):>_/N<:(_,_)
with {
  gainEnvelope = en.adsr(attack,decay,sustain,release,gate )*gain;
};
attack = hslider("attack", 0, 0, 1, 0.005);
decay = hslider("decay", 1, 0, 4, 0.01);
sustain = hslider("sustain", 1, 0, 1, 0.01) ;
release = hslider("release", 1, 0, 4, 0.01);

attSec=0.001; //  2 ms attack time
decT60=0.010; // 10 ms decay-to-sustain time
susLvl=1;  // Sustain level = 0.8
relT60=0.010; // 10 ms release (decay-to-zero) time
// we can ony shift octaves up, not down so start with the lowest possibly usefull octave divider:
// we want any note to be able to turn subsonic
// midi 127 = 12543.9 Hz
// 12543.9/(2^10) = 12.249902 Hz
baseOct = 10;
minFreq = maxFreq/pow2(baseOct); // 12.249902;
maxFreq = 12543.9;
// baseOct = 5;

// freq = hslider("freq", 110, 55, 880, 1):si.smoo;
targetFreq = hslider("target freq", 110, minFreq, 880, 1): si.polySmooth(gate,0.999,1);//:si.smoo;
level = hslider("level", 0, -60, 0, 1): si.polySmooth(gate,0.999,1):ba.db2linear;

xfade(x,a,b) = a*(x*-1+1)+b*x;

slaveSaw(fund,index) = os.CZsaw(fund, index);

pow2(i) = 1<<int(i);
// same as:
// pow2(i) = int(pow(2,i));
// but in the block diagram, it will be displayed as a number, instead of a formula
