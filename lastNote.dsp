declare author "Bart Brouns";
declare license "AGPLv3";
declare name "lastNote";
declare options "[midi:on]";
import("stdfaust.lib");
// import("/home/bart/source/edgeofchaos/edgeofchaos.lib");
///////////////////////////////////////////////////////////////////////////////
//           give the number of the last note played                         //
///////////////////////////////////////////////////////////////////////////////
phase = hslider("phase", 0, 0, 1, stepsize);
// traditional faust synth:
// freq = midiGroup(hslider("freq",440,0,24000,0.0001)) :new_smooth( ba.tau2pole(portamento));
// gain = midiGroup(hslider("gain",0.5,0,1,stepsize));
// gate = midiGroup(button("gate"));

// workaround for proper monophonic handling, increases compile-time massively and CPU usage with +/- 5%:
// freq = lastNote:ba.pianokey2hz :new_smooth(ba.tau2pole(portamento));
// freq = lastNote:ba.pianokey2hz :si.smooth(gate' * ba.tau2pole(portamento));

freq(lastNote) = lastNote:ba.pianokey2hz : enabled_smooth(gate(lastNote)' & gate(lastNote) , ba.tau2pole(portamento));
gain(lastNote) = (vel(lastNote)/127); // increases the cpu-usage, from 7% to 11%
gate(lastNote) = gain(lastNote)>0;

// gate = vel(lastNote)>0;
// gain = (nrNotesPlaying>0); // no velocity, 7% cpu

// TODO: why this doesn't work,only gate works.
// from https://github.com/timowest/analogue/blob/master/faust/midi.dsp
// freq = hslider("/h:midi/pitch", 64, 32, 100, 1):ba.pianokey2hz;
// gain = hslider("/h:midi/gain", 1, 0, 1, 0.01);
// gate = button("/h:midi/gate");

///////////////////////////////////////////////////////////////////////////////
//                                  process                                  //
///////////////////////////////////////////////////////////////////////////////

process =
  // oscillator(0,fund,gai,gat);
  // CZparams(0,gat,gai) ;
  // CZsynth;
  CZsynthVectorOsc;
// vectorMixer(hslider("ab", 0, 0, 1, stepsize),hslider("cd", 0, 0, 1, stepsize));
// vectorOsc(0,fund,gat,gai,ab,cd) ;
// envMixer(CZsawGroup,levelGroup,1,oscillatorLevel);
fund = 0.1;
gai=0.2;
gat=0.3;

ab = hslider("ac/bd", 0, 0, 1, stepsize);
cd = hslider("ab/cd", 0, 0, 1, stepsize);


mono  =  envM*envMidmaster+midAmount;
left  = (envM*envMidmaster + envS*envSideMaster) + midAmount + sideAmount;
right = (envM*envMidmaster - envS*envSideMaster) + midAmount - sideAmount;

// envMixer(group,subGroup,0,param,gate,gain) =
// par(j, nrEnvelopes, group(offset(envLevel(subGroup,j)),i), envelope(j,gate,gain))
// :mixer(nrEnvelopes,1,1)
// * group(offset(subGroup(masterGroup(param)),i))
// + group(offset(subGroup(param),i)) ;

envMixer(group,subGroup,i,param,gate,gain) =
  ((((env(M)*envMaster(M)) , (env(S)*envMaster(S)))) :op(i))
+ ((amount(M), amount(S)) : op(i))
with {
  env(MSgroup) = par(j, nrEnvelopes,
                     (group(MSgroup(envLevel(subGroup,j))):si.smooth(0.999))
                     , envelope(j,gate,gain))
                 :mixer(nrEnvelopes,1,1);
  envMaster(MSgroup) = group(MSgroup(subGroup(masterGroup(param)))):si.smooth(0.999);
  amount(MSgroup) = group(MSgroup(subGroup(param))):si.smooth(0.999);
  M = mainGroup;
  S = offsetGroup;
  op(0) = +;
op(1) = -;
};


OLDenvMixer(group,subGroup,i,param,gate,gain) =
  par(j, nrEnvelopes, group(offset(envLevel(subGroup,j)),i), envelope(j,gate,gain))
  :mixer(nrEnvelopes,1,1)
* group(offset(subGroup(masterGroup(param)),i))
+ group(offset(subGroup(param),i)) ;

envLevel(subGroup,i) = subGroup(vgroup("[-1]envelope mixer", hslider("envLevel %i", 0, -1, 1, stepsize)));

///////////////////////////////////////////////////////////////////////////////
//                                    GUI                                    //
///////////////////////////////////////////////////////////////////////////////

//groups///////////////////////////////////////////////////////////////////////

tabs(x) = tgroup("CZsynth", x);
oscillatorGroup(x) = tabs(vgroup("[00]oscillators", x));
// envelopeGroup(x) = tabs(vgroup("[01]envelope", x));
envelopeGroup(i,x) = tabs(vgroup("envelopes", hgroup("[%i]envelope %i", x)));
midiGroup(x) = tabs(hgroup("[99]midi", x));
mainGroup(x) = stereoGroup(vgroup("[0]main", x));
offsetGroup(x) = stereoGroup(vgroup("[1]L-R offset", x));
stereoGroup(x) = tgroup("stereo", x);
globalGroup(x)        = tabs(hgroup("[-1]global", x));
sineGroup(x)          = tabs(hgroup("[0]sine", x));
CZsawGroup(x)         = tabs(hgroup("[1]CZ saw", x));
CZsquareGroup(x)      = tabs(hgroup("[2]CZ square", x));
CZpulseGroup(x)       = tabs(hgroup("[3]CZ pulse", x));
CZsinePulseGroup(x)   = tabs(hgroup("[4]CZ sinePulse", x));
CZhalfSineGroup(x)    = tabs(hgroup("[5]CZ halfSine", x));
CZresSawGroup(x)      = tabs(hgroup("[6]CZ resSaw", x));
CZresTriangleGroup(x) = tabs(hgroup("[7]CZ resTriangle", x));
CZresTrapGroup(x)     = tabs(hgroup("[8]CZ resTrap", x));
sawGroup(x)           = tabs(hgroup("[9]saw", x));


allpassGroup(x)  = preFilterGroup(hgroup("[0]allpass", x));
ms20Group(x)     = preFilterGroup(hgroup("[1]ms20", x));
oberheimGroup(x) = preFilterGroup(hgroup("[2]oberheim", x));
normFreqGroup(x) = preFilterGroup(hgroup("[3]freq", x));
QGroup(x)        = preFilterGroup(hgroup("[4]Q", x));

levelGroup(x) = oscGroup(hgroup("[0]level", x));
indexGroup(x) = oscGroup(hgroup("[1]index", x));
octGroup(x)   = oscGroup(hgroup("[2]oct", x));

oscGroup(x)       = hgroup("oscillator",x);
preFilterGroup(x) = hgroup("preFilter",x);

masterGroup(x) = hgroup("master",x);

//sliders//////////////////////////////////////////////////////////////////////
masterPhase = hslider("masterPhase", 0, -1, 1, stepsize) :new_smooth(0.999);
portamento = hslider("portamento[scale:log]", 0, 0, 1, stepsize);

oscillatorLevel = vslider("[0]Level", 0, -1, 1, stepsize);
oscillatorIndex = vslider("[1]index", 0, 0, 1, stepsize);
oscillatorRes   = vslider("[1]res", 0, 0, 64, stepsize);
oct             = vslider("oct", 0, minOct, maxOct, stepsize);

// sawIndex = sawGroup(oscillatorIndex);
// pulseIndex = pulseGroup(oscillatorIndex);
attack(i)  = envelopeGroup(i,hslider("[0]attack", 0, 0, maxAttack, stepsize));
decay(i)   = envelopeGroup(i,hslider("[1]decay", 0.1, 0, maxDecay, stepsize));
sustain(i) = envelopeGroup(i,hslider("[2]sustain", 0.8, 0, 1, stepsize));
release(i) = envelopeGroup(i,hslider("[3]release", 0.1, 0, maxRelease, stepsize));


lfo_amount = hslider("lfo amount", 0, 0, 1, stepsize):new_smooth(0.999);
// velocity(i) = midiGroup(select2(i>=0, 0, hslider("velocity of note %i [midi:key %i ]", 0, 0, 127, 1)));
// velocity(-1) = 0;
// velocity(i) = midiGroup(hslider("velocity of note %i [midi:key %i ]", 0, 0, 127, 1));
velocity(i) = VEL(i:max(-1):int) with {
  VEL =
    case {
      (-1) => 0;
      (i) => midiGroup(midiTabs(tabNr(i),vslider("velocity of note %i [midi:key %i ]", 0, 0, 127, 1)));
    };
  midiTabs(tabNr,x) = tgroup("velocity",hgroup("v %tabNr", x));
  tabNr(i) = (i/nrTabs):floor;
  nrTabs = int((nrNotes+1)/16)+1;
};


// pre-filter /////////////////////////////////////////////////////////////////
LPfreq = hslider("LPfreq[scale:log]", 24000, 20, 24000, 1);

filterBPlevel = vslider("filterBPlevel", 1, 0, 1, stepsize);
allpassLevel  = vslider("level", 0, 0, 1, stepsize);
ms20level     = vslider("level", 0, 0, 1, stepsize);
oberheimLevel = vslider("level", 0, 0, 1, stepsize);
normFreq      = vslider("normFreq", 1, 0, 1, stepsize);
Q             = vslider("Q", 1, stepsize, 10, stepsize);
///////////////////////////////////////////////////////////////////////////////
//                               implementation                              //
///////////////////////////////////////////////////////////////////////////////

octaver(fund,oscillator,params,oct) =
  (
    ((f0,params):oscillator)
  , ((f1,params):oscillator)
// ):it.interpolate_linear(oct:ma.decimal)
  ):it.interpolate_linear(oct:abs%1)
with {
  f0 = fund:octaveSwitcher(oct:floor+((oct<0) & (oct!=(oct:floor))));
  f1 = fund:octaveSwitcher(oct:floor+(oct>0));
  octaveSwitcher(oct) = _*(octaveMultiplier(oct)/minOctMult)%1;
};

octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,oscillator,params,oct) =
  (
    ((f0:preFilter(allpassLevel,ms20level,oberheimLevel,normFreq,Q),params):oscillator)
  , ((f1:preFilter(allpassLevel,ms20level,oberheimLevel,normFreq,Q),params):oscillator)
// ):it.interpolate_linear(oct:ma.decimal)
  ):it.interpolate_linear(oct:abs%1)
with {
  f0 = fund:octaveSwitcher(oct:floor+((oct<0) & (oct!=(oct:floor))));
  f1 = fund:octaveSwitcher(oct:floor+(oct>0));
  octaveSwitcher(oct) = _*(octaveMultiplier(oct)/minOctMult)%1;
};

octaverFilter_No_Osc(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,oct) =
  (
    (f0:preFilter(allpassLevel,ms20level,oberheimLevel,normFreq,Q))
  , (f1:preFilter(allpassLevel,ms20level,oberheimLevel,normFreq,Q))
  , (oct:abs%1)
  )
with {
  f0 = fund:octaveSwitcher(oct:floor+((oct<0) & (oct!=(oct:floor))));
  f1 = fund:octaveSwitcher(oct:floor+(oct>0));
  octaveSwitcher(oct) = _*(octaveMultiplier(oct)/minOctMult)%1;
};


octaveMultiplier	=
  int<:
  (
    (_ <0) / pow(2,abs),
    (_==0),
    (_ >0) * pow(2,_)
  ):>_;

minOctMult = minOct:octaveMultiplier;

smoothDelayedBy(d, s) = *(1.0 - sd) : + ~ *(sd)
with {
  sd = s * ((1-1@d) < 1) ;
};

CZsynth =
  (lastNote<:(master,gate,gain)) :
  (si.bus(3)<:si.bus(6)) :
  par(i, 2, CZsynthMono(i));

CZsynthSingleOsc =
  (lastNote<:(master,gate,gain)) :
  (si.bus(3)<:si.bus(6)) :
  par(i, 2, CZsynthMonoSingleOsc(i));

CZsynthVectorOsc =
  (lastNote<:(master,gate,gain)) :
  (si.bus(3)<:si.bus(6)) :
  par(i, 2, CZsynthMonoVectorOsc(i));

CZsynthMono(i,fund,gate,gain) =
  (oscillators(i,fund,gate,gain) : filters(i))
* envelope(-1,gate,gain);

CZsynthMonoSingleOsc(i,fund,gate,gain) =
  (oscillator(i,fund,gate,gain) : filters(i))
* envelope(-1,gate,gain);

CZsynthMonoVectorOsc(i,fund,gate,gain) =
  (vectorOsc(i,fund,gate,gain,ab,cd) : filters(i))
* envelope(-1,gate,gain);

oscillators(i,fund,gate,gain) =
  (
    (preFilterOct(i,fund,gate,gain)  <: ro.interleave(3,9))
   ,CZparams(i,gate,gain)
  )
  : (ro.crossNM(3*9,9),si.bus(9))
  : ro.interleave(9,5) //9* osc by 9 params per osc
  :
  (
    (!,sinPPF)
  , ((_,CZsawPPF):enableIfVolume)
  , ((_,CZsquarePPF):enableIfVolume)
  , ((_,CZpulsePPF):enableIfVolume)
  , ((_,CZsinePulsePPF):enableIfVolume)
  , ((_,CZhalfSinePPF):enableIfVolume)
  , ((_,CZresSawPF):enableIfVolume)
  , ((_,CZresTrianglePF):enableIfVolume)
  , ((_,CZresTrapPF):enableIfVolume)
  )
  :fallbackMixer(8,1,1)
;

oscillator(i,fund,gate,gain) =
  (
    preFilterOct(i,fund,gate,gain)
   ,envMixer(globalGroup,indexGroup,i,oscillatorIndex,gate,gain)
  )
  : oscillatorSelector
;

vectorOsc(i,fund,gate,gain,ab,cd) =
  (
    (
      preFilterOct(i,fund,gate,gain)<:
      (
        (si.bus(3),envMixer(A,indexGroup,i,oscillatorIndex,gate,gain))
       ,(si.bus(3),envMixer(B,indexGroup,i,oscillatorIndex,gate,gain))
       ,(si.bus(3),envMixer(C,indexGroup,i,oscillatorIndex,gate,gain))
       ,(si.bus(3),envMixer(D,indexGroup,i,oscillatorIndex,gate,gain))
      )
    )
    :
    (
      A(oscillatorSelector)
    , B(oscillatorSelector)
    , C(oscillatorSelector)
    , D(oscillatorSelector)
    )
    : vectorMixer(ab,cd)
  ) with {
  A(x) = tabs(hgroup("[100]A", x));
  B(x) = tabs(hgroup("[101]B", x));
  C(x) = tabs(hgroup("[102]C", x));
  D(x) = tabs(hgroup("[103]D", x));
  };

/*

bottomLeft = 0,0 = a
topLeft = 0,1
bottomRight = 1,0
topRight = 1,1


*/


vectorMixer(ab,cd) =
  // meta
  // (
  // _*( invert(ab)*(wrap(cd):hbargraph("wrap(cd)", 0, 1)) )
    // , _*( ab*(wrap(cd)) )
    // , _*( (wrap(ab):hbargraph("wrap(ab)", 0, 1))*invert(cd) )
    // , _*( (wrap(ab))*(cd) )

  // ):>_
  si.bus(4)
  : (
  (si.interpolate(ab))
, (si.interpolate(ab))
  )
  : (si.interpolate(cd))
with {
  ins = inputs(oscil);
  meta =
    (ab*2-(ab*2:ma.frac))
+
(cd*2-(cd*2:ma.frac))
  ;
  // wrap(x) = x*2-(x*2:ma.frac);
  wrap(x) = select2( x>0.5
                   , x*2
                   ,2-(x*2));
  invert(x) = (x*-1)+1;
};

// TODO: optional latch for osc type change when vol is not 0
// TODO: phase coherent pitchdrop for kicks: start envelope at phase=-1000
// TODO: crossfade to blep at high index, high being dependant on note

/*

F = ([A*(64-X)+B*(64+X)]*(64-|Y|)+[C*(64+Y)+D*(64-Y)]*(64-|X|))/128/64

F =
[A*(64-X)+B*(64+X)]*(64-|Y|)
+
[C*(64+Y)+D*(64-Y)]*(64-|X|)


a = invert(ab)*wrap(cd)
b = ab*wrap(cd)
c = wrap(ab)*invert(cd)
d = wrap(ab)*cd

meta =
if cd = 0.5: 0 else 1
if ab = 0.5: 1 else 0

ab = 1;
cd = 0.5;
a = 0;
b = 1;
c = 0;
d = 0;

ab = 0;
cd = 0.5;
a = 1;
b = 0;
c = 0;
d = 0;

ab = 0.5;
cd = 0;
a = 0;
b = 0;
c = 1;
d = 0;

ab = 0;
cd = 0;
a = 0.5;
b = 0;
c = 0.5;
d = 0;

ab = 1;
cd = 1;
a = 0;
b = 0.5;
c = 0;
d = 0.5;

ab = 0.25;
cd = 0.75;
a = ;
b = ;
c = ;
d = ;

ab = 0.75;
cd = 0.25;
a = ;
b = ;
c = ;
d = ;
*/

oscillatorSelector =
  si.bus(4)<:
  (
    sinPPF
  , CZsawPPF
  , CZsquarePPF
  , CZpulsePPF
  , CZsinePulsePPF
  , CZhalfSinePPF
  , CZresSawPF
  , CZresTrianglePF
  , CZresTrapPF
  ) : enableOneOfN(9,vslider("type", 0, 0, 8, 1));

enableOneOfN(maxN,N) = par(i, maxN, control(i==N)*(i==N)):>_;

CZparams(i,gate,gain) =
  (
    0,0 // for routing
    , oscParamsI(CZsawGroup,i,gate,gain)
    , oscParamsI(CZsquareGroup,i,gate,gain)
    , oscParamsI(CZpulseGroup,i,gate,gain)
    , oscParamsI(CZsinePulseGroup,i,gate,gain)
    , oscParamsI(CZhalfSineGroup,i,gate,gain)
    , oscParamsR(CZresSawGroup,i,gate,gain)
    , oscParamsR(CZresTriangleGroup,i,gate,gain)
    , oscParamsR(CZresTrapGroup,i,gate,gain)
  )
  : ro.interleave(2,9);

CZparams_oct(i,gate,gain) =
  (
    0,0,envMixer(globalGroup,octGroup,i,oct,gate,gain) // for routing
    , oscParamsI_oct(CZsawGroup,i,gate,gain)
    , oscParamsI_oct(CZsquareGroup,i,gate,gain)
    , oscParamsI_oct(CZpulseGroup,i,gate,gain)
    , oscParamsI_oct(CZsinePulseGroup,i,gate,gain)
    , oscParamsI_oct(CZhalfSineGroup,i,gate,gain)
    , oscParamsR_oct(CZresSawGroup,i,gate,gain)
    , oscParamsR_oct(CZresTriangleGroup,i,gate,gain)
    , oscParamsR_oct(CZresTrapGroup,i,gate,gain)
  )
  : ro.interleave(3,9);

oldCZparams(i) =
  (
    0,0 // for routing
    , CZsawGroup(indexParam(i))
// , (envMixer(CZsawGroup),envMixer(CZsawGroup))
// , oscParams(CZsawGroup,i)
    , CZsquareGroup(indexParam(i))
    , CZpulseGroup(indexParam(i))
    , CZsinePulseGroup(indexParam(i))
    , CZhalfSineGroup(indexParam(i))
    , CZresSawGroup(resParam(i))
    , CZresTriangleGroup(resParam(i))
    , CZresTrapGroup(resParam(i))
  )
  : ro.interleave(2,9);

preFilterParamsLocal(i,gate,gain) =
  (
    oscPreFilterParams(sineGroup,i,gate,gain)
  , oscPreFilterParams(CZsawGroup,i,gate,gain)
  , oscPreFilterParams(CZsquareGroup,i,gate,gain)
  , oscPreFilterParams(CZpulseGroup,i,gate,gain)
  , oscPreFilterParams(CZsinePulseGroup,i,gate,gain)
  , oscPreFilterParams(CZhalfSineGroup,i,gate,gain)
  , oscPreFilterParams(CZresSawGroup,i,gate,gain)
  , oscPreFilterParams(CZresTriangleGroup,i,gate,gain)
  , oscPreFilterParams(CZresTrapGroup,i,gate,gain)
  )
  : ro.interleave(5,9);

preFilterOct(i,fund,gate,gain) =
  (
    (fund,oscPreFilterParams(globalGroup,i,gate,gain),oct)
    : octaverFilter_No_Osc //(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,oct)
  )
;

oscPreFilterParams(group,i,gate,gain) =
  envMixer(group,allpassGroup,i,allpassLevel,gate,gain)
, envMixer(group,ms20Group,i,ms20level,gate,gain)
, envMixer(group,oberheimGroup,i,oberheimLevel,gate,gain)
, (envMixer(group,normFreqGroup,i,normFreq,gate,gain):max(0):min(1))
, (envMixer(group,QGroup,i,Q,gate,gain):max(stepsize):min(10));

oscParamsI_oct(group,i,gate,gain) =
  envMixer(group,levelGroup,i,oscillatorLevel,gate,gain)
, envMixer(group,indexGroup,i,oscillatorIndex,gate,gain)
, envMixer(globalGroup,octGroup,i,oct,gate,gain);

oscParamsR_oct(group,i,gate,gain) =
  envMixer(group,levelGroup,i,oscillatorLevel,gate,gain)
, envMixer(group,indexGroup,i,oscillatorRes,gate,gain)
, envMixer(group,octGroup,i,oct,gate,gain);

oscParamsI(group,i,gate,gain) =
  envMixer(group,levelGroup,i,oscillatorLevel,gate,gain)
, envMixer(group,indexGroup,i,oscillatorIndex,gate,gain);

oscParamsR(group,i,gate,gain) =
  envMixer(group,levelGroup,i,oscillatorLevel,gate,gain)
, envMixer(group,indexGroup,i,oscillatorRes,gate,gain);

indexParam(i) = offset(oscillatorLevel,i)
              , (offset(oscillatorIndex,i));

resParam(i) = offset(oscillatorLevel,i)
            , (offset(oscillatorRes,i));

OLDoscillators(i,fund) =
  (fund :preFilter
         <:si.bus(9)):
  (
    (_*2*ma.PI:sin)
  ,
    CZsawGroup  ((
                  offset(oscillatorLevel,i)
                , ((_, offset(oscillatorIndex,i)):CZsawP)
    ))
  , CZsquareGroup  ((
                     offset(oscillatorLevel,i)
                    ,((_, offset(oscillatorIndex,i)):CZsquareP)
  ))
  , CZpulseGroup((
                  offset(oscillatorLevel,i)
                , ((_, offset(oscillatorIndex,i)):CZpulseP)
  ))
  , CZsinePulseGroup((
                      offset(oscillatorLevel,i)
                    , ((_, offset(oscillatorIndex,i)):CZsinePulseP)
  ))
  , CZhalfSineGroup((
                     offset(oscillatorLevel,i)
                   , ((_, offset(oscillatorIndex,i)):CZhalfSineP)
  ))
  , CZresSawGroup  ((
                     offset(oscillatorLevel,i)
                   , ((_, offset(oscillatorRes,i)):CZresSaw)
  ))
  , CZresTriangleGroup  ((
                          offset(oscillatorLevel,i)
                        , ((_, offset(oscillatorRes,i)):CZresTriangle)
  ))
  , CZresTrapGroup  ((
                      offset(oscillatorLevel,i)
                    , ((_, offset(oscillatorRes,i)):CZresTrap)
  ))
  )
  :fallbackMixer(8,1,1) ;


filters(i) = _;
envelope(i,gate,gain) =  adsreg(attack(i),decay(i),sustain(i),release(i),gate,gain);
// envelope(i) =  _*adsre(attack(i),decay(i),sustain(i),release(i),gate);
// envelope(i) = _*en.adsre(attack(i),decay(i),sustain(i),release(i),gate);

// master = lf_sawpos_reset(freq,reset) ;
master(lastNote) = lf_sawpos_phase_reset(freq(lastNote)*minOctMult,masterPhase,reset(lastNote)) ;
reset(lastNote) = gate(lastNote):ba.impulsify;

offset(param,i) = mainGroup(param)+(offsetGroup(param) * select2(i,1,-1)) :new_smooth(0.999);

preFilter(allpassLevel,ms20level,oberheimLevel,normFreq,Q) =
  _<:
  (
    _
  , allpassLevel , fi.allpassnn(1,normFreq/2*ma.PI)
  , ms20level , ve.korg35LPF(normFreq,Q)
  , oberheimLevel , ve.oberheimLPF(normFreq,Q)
  )
  :fallbackMixer(3,1,1);

// oneSumMixer(nrInChan,nrOutChan,nrSends) =
// par(i, nrInChan, si.bus(nrSends),si.bus(nrOutChan)) : mixer(nrInChan,nrOutChan,nrSends) : par(i, nrSends, si.bus(nrOutChan));


lfo = 0.5*(1+os.osc(0.5));
// vel(x) = chooseFromFixed(nrNotes,velocity,x);
vel(x) = x:chooseFromFixed(nrNotes,velocity);
//par(i, nrNotes, velocity(i)*(i==x)):>_ ;

enableIfVolume =
  si.bus(2) <:
  ((_,!)
  ,(ro.cross(2) :(_,_!=0):control)) ;
// enableIfVolume = si.bus(2);
///////////////////////////////////////////////////////////////////////////////
//                                still to PR:                               //
///////////////////////////////////////////////////////////////////////////////

adsre(attT60,decT60,susLvl,relT60,gate) = envelope with {
  ugate = gate>0;
  samps = ugate : +~(*(ugate)); // ramp time in samples
  attSamps = int(attT60 * ma.SR);
  // the first sample of each note is alwaus the attack phase, also when attSamps==0
  attPhase = (samps<attSamps) |  (ugate:ba.impulsify);
  // attPhase = (samps<attSamps) | ((attSamps==0) & (ugate:ba.impulsify));
  target = select2(ugate, 0.0,
                   select2(attPhase, (susLvl)*float(ugate), ugate));
  t60 = select2(ugate, relT60, select2(attPhase, decT60, attT60));
  pole = ba.tau2pole(t60/6.91);
  envelope = target : si.smooth(pole);
};

adsreg(attT60,decT60,susLvl,relT60,gate,gain) = envelope with {
  ugate = gate>0;
  samps = ugate : +~(*(ugate)); // ramp time in samples
  attSamps = int(attT60 * ma.SR);
  // the first sample of each note is alwaus the attack phase, also when attSamps==0
  attPhase = (samps<attSamps) |  (ugate:ba.impulsify);
  // attPhase = (samps<attSamps) | ((attSamps==0) & (ugate:ba.impulsify));
  target = select2(ugate, 0.0,
                   select2(attPhase, (susLvl)*float(ugate), ugate)) * gain;
  t60 = select2(ugate, relT60, select2(attPhase, decT60, attT60));
  pole = ba.tau2pole(t60/6.91);
  envelope = target : si.smooth(pole);
};
myBus(0) = 0:!;
myBus(i) = si.bus(i);

sumN(n) = si.bus(n):>_;
minN(n) = opWithNInputs(min,n);
maxN(n) = opWithNInputs(max,n);
meanN(n) = sumN(n)/n;
RMSn(n) = par(i, n, pow(2)) : meanN(n) : sqrt;

opWithNInputs =
  case {
    (op,0) => 0:!;
        (op,1) => _;
    (op,2) => op;
    (op,N) => (opWithNInputs(op,N-1),_) : op;
  };

chooseFromFixed(maxN,expression,N) = par(i, maxN, expression(i)*(i==N)):>_;


// https://github.com/grame-cncm/faustlibraries/pull/44#issuecomment-651245377
new_smooth(s) = si.smooth(s * ((1-1') < 1) );
enabled_smooth(e,s) = si.smooth(s * e );

///////////////////////////////////////////////////////////////////////////////
//                               mixers from PR                              //
///////////////////////////////////////////////////////////////////////////////

//  (si.bus(nrSends),_) : monoMixerChannel(nrSends) : si.bus(nrSends);
monoMixerChannel(nrSends) =
  (
    si.bus(nrSends)
  , (_<:si.bus(nrSends))
  )
  :ro.interleave(nrSends,2)
  :par(i,nrSends,_*_);

oneSumMonoMixerChannel(nrSends) =
  (
    (si.bus(nrSends)
     <: (
      // par(i, nrSends, min(nrSends*ma.MIN))
      // ,(sumN(nrSends) : min(nrSends*nrSends*ma.MIN) <: si.bus(nrSends) )
      si.bus(nrSends)
     ,(sumN(nrSends) <: si.bus(nrSends) )
     )
     :  ro.interleave(nrSends,2) : par(i, nrSends, _/_)
    )
  , (_<:si.bus(nrSends))
  )
  :ro.interleave(nrSends,2)
  :par(i,nrSends,_*_);

//(si.bus(nrSends),si.bus(nrOutChan)) : multiMixerChannel(nrOutChan,nrSends) : par(i, nrSends, si.bus(nrOutChan));
multiMixerChannel(nrOutChan,nrSends) =
  si.bus(nrOutChan+nrSends)
  <:par(i,nrOutChan,sel(nrOutChan,nrSends,i))
  :par(i,nrOutChan,monoMixerChannel(nrSends))
  :ro.interleave(nrSends,nrOutChan)
with {
  sel(nrOutChan,nrSends,i) = (par(i,nrSends,_),par(i,nrOutChan,!),(ba.selector(i+nrSends,nrOutChan+nrSends)));
};

mixer(nrInChan,nrOutChan,nrSends) =
  par(i,nrInChan,multiMixerChannel(nrOutChan,nrSends)):ro.interleave(nrSends*nrOutChan,nrInChan):mix
with {
  mix=par(i,nrOutChan*nrSends,(si.bus(nrInChan):>_));
};

// par(i, nrInChan, sendsBus,outBus)
// : mixer(nrInChan,nrOutChan,nrSends)
// : par(i, nrSends, outBus);
fallbackMixer(nrInChan,nrOutChan,nrSends,fallBack) =
  (
    (ro.interleave(nrOutChan+nrSends,nrInChan)
     : (FallbackGains
       , ro.interleave(nrInChan, nrOutChan)
     ))
  , fallBack
  )
  :
  (si.bus((nrInChan+1)*nrSends), ro.interleave(nrOutChan,nrInChan+1) )
  :
  // (par(i, nrInChan+1, ro.interleave(nrOutChan+nrSends,1)))
  // ro.interleave(nrOutChan+nrSends,nrInChan+1)
  ro.interleave(nrInChan+1,nrOutChan+nrSends)
  : mixer(nrInChan+1,nrOutChan,nrSends)
with {
  mix=par(i,nrOutChan*nrSends,(si.bus(nrInChan):>_));
  inBus = si.bus(nrInChan);
  outBus = si.bus(nrOutChan);
  sendsBus = si.bus(nrSends);
  FallbackGains =
    (
      par(i,nrSends,
          (inBus
           <: (
            par(i, nrInChan,
                (_<:(_<0,(abs:max(nrInChan*ma.MIN)<:(_,_)))) : select2(_,_,_*-1)
            )
          , ((par(i, nrInChan, abs):sumN(nrInChan)) <: (max(nrInChan*nrInChan*ma.MIN)<:inBus)
            , _
          )
           )
           :((ro.interleave(nrInChan,2):par(i, nrInChan, /))
            , (0<:(inBus))
            , (min(1)<:(inBus,_))
           )
           : ((ro.interleave(nrInChan,3)
               : par(i, nrInChan, ro.cross(3) : it.interpolate_linear)
           ),_*-1+1)
          )
      )
    )
  ;
};
oneSumMixer(nrInChan,nrOutChan,nrSends) =
  // ro.interleave(nrInChan,nrOutChan+nrSends)
  // par(i, nrInChan, sendsBus,outBus)
  ro.interleave(nrOutChan+nrSends,nrInChan)
  :
  (oneSumGains
  , par(i, nrInChan, outBus)
  )
  :
  // par(i, nrSends, ro.interleave(nrOutChan,nrInChan))
  ro.interleave(nrInChan,nrOutChan+nrSends)

  : mixer(nrInChan,nrOutChan,nrSends)
// : par(i,nrInChan,multiMixerChannel(nrOutChan,nrSends))
// : ro.interleave(nrSends*nrOutChan,nrInChan)
// : mix
with {
  mix=par(i,nrOutChan*nrSends,(si.bus(nrInChan):>_));
  inBus = si.bus(nrInChan);
  outBus = si.bus(nrOutChan);
  sendsBus = si.bus(nrSends);
  oneSumGains =

    (
      par(i,nrSends,
          (inBus
           <: (
            par(i, nrInChan,
                (_<:(_<0,(abs:max(nrInChan*ma.MIN)<:(_,_)))) : select2(_,_,_*-1)
            )
          , (par(i, nrInChan, abs):sumN(nrInChan):max(nrInChan*nrInChan*ma.MIN)<:inBus)
           )
          )
          :ro.interleave(nrInChan,2):par(i, nrInChan, /)
      ))
  ;
};
///////////////////////////////////////////////////////////////////////////////
//                                  lastNote                                 //
///////////////////////////////////////////////////////////////////////////////

// uniqueify makse sure that when multiple new notes are started simultaneously, each get's a unique index
// TODO: make the uniqueify function only affect the new notes
nrNotesPlaying = 0: seq(i, nrNotes, noteIsOn(i),_:+);
noteIsOn(i) = velocity(i)>0;

lastNote =
  par(i, nrNotes, i, index(i))
// , ((par(i, nrNotes, index(i)),uniqueIfy):ro.interleave(nrNotes,2):par(i, nrNotes, +))
  :find_max_index(nrNotes)
  :ba.sAndH(nrNotesPlaying>0)
;
// with {
// an index to indicate the order of the note
// it adds one for every additional note played
// it resets to 0 when there are no notes playing
// assume multiple notes can start at once
orderIndex = ((_+nrNewNotes) * (nrNotesPlaying>1))~_;
nrNewNotes = ((nrNotesPlaying-nrNotesPlaying')):max(0);

// the order index of note i
// TODO: when multiple notes start at the same time, give each a unique index
index(i) = orderIndex:(select2(noteStart(i),_,_)
                       :select2(noteEnd(i)+(1:ba.impulsify),_,-1))~_;

// we use this instead of:
// hslider("frequency[midi:keyon 62]",0,0,nrNotes,1)
// because keyon can come multiple times, and we only want the first
noteStart(i) = noteIsOn(i):ba.impulsify;
noteEnd(i) = (noteIsOn(i)'-noteIsOn(i)):max(0):ba.impulsify;
//or do we?
// noteStart(i) = (hslider("keyon[midi:keyon %i]",0,0,nrNotes,1)>0) :ba.impulsify;
// ERROR : path '/lastNote/keyon' is already used
// noteEnd(i) = ((hslider("keyon[midi:keyon %i]",0,0,nrNotes,1)>0)'-(hslider("keyon[midi:keyon %i]",0,0,nrNotes,1)>0)):max(0):ba.impulsify;
// at the very least, the first implementation of noteStart(i) doesn't add another 127 sliders

// from Julius Smith's acor.dsp:
index_comparator(n,x,m,y) = select2((x>y),m,n), select2((x>y),y,x); // compare integer-labeled signals
// take N number-value pairs and give the number with the maximum value
find_max_index(N) = seq(i,N-2, (index_comparator,si.bus(2*(N-i-2)))) : index_comparator :(_,!);

uniqueIfy =
  (0:seq(i, nrNotes, myBus(i),(_-(noteIsOn(i-1)*(nrNewNotes>1))<:(_,_)) ):(si.bus(nrNotes),!));

// };
//////////////////////////////////////////////////////////////////////////////
//                            from @idle on slack                            //
//////////////////////////////////////////////////////////////////////////////


svf = environment {
	    svf(T,F,Q,G) = tick ~ (_,_) : !,!,_,_,_ : si.dot(3, mix)
	    with {
		tick(ic1eq, ic2eq, v0) =
		  2*v1 - ic1eq,
		  2*v2 - ic2eq,
		  v0, v1, v2
		with {
		v1 = ic1eq + g *(v0-ic2eq) : /(1 + g*(g+k));
		v2 = ic2eq + g * v1;
		};
		A = pow(10.0, G / 40.0);
		g = tan(F * ma.PI / ma.SR) : case {
			  (7) => /(sqrt(A));
			  (8) => *(sqrt(A));
			  (t) => _;
} (T);
		k = case {
			  (6) => 1/(Q*A);
			  (t) => 1/Q;
} (T);
		mix = case {
			    (0) => 0, 0, 1;
			    (1) => 0, 1, 0;
			    (2) => 1, -k, -1;
			    (3) => 1, -k, 0;
			    (4) => 1, -k, -2;
			    (5) => 1, -2*k, 0;
			    (6) => 1, k*(A*A-1), 0;
			    (7) => 1, k*(A-1), A*A-1;
			    (8) => A*A, k*(1-A)*A, 1-A*A;
} (T);
	    };
	    lp(f,q)		= svf(0, f,q,0);
	    bp(f,q)		= svf(1, f,q,0);
	    hp(f,q)		= svf(2, f,q,0);
	    notch(f,q)	= svf(3, f,q,0);
	    peak(f,q)	= svf(4, f,q,0);
	    ap(f,q)		= svf(5, f,q,0);
	    bell(f,q,g)	= svf(6, f,q,g);
	    ls(f,q,g)	= svf(7, f,q,g);
	    hs(f,q,g)	= svf(8, f,q,g);
};






//////////////////////////////////////////////////////////////////////////////
//           https://github.com/grame-cncm/faustlibraries/pull/47           //
//////////////////////////////////////////////////////////////////////////////

CZ =
  environment {
    saw(fund, index) = sawChooseP(fund, index, 0);
    sawP(fund, index) = sawChooseP(fund, index, 1);
    sawChooseP(fund, index, p) =
      (((fnd(fund,allign,p)*((.5-tmp)/tmp)),(-1*fnd(fund,allign,p)+1)*((.5-tmp)/(1-tmp))):min+fnd(fund,allign,p))*2*ma.PI:cos
    with {
      tmp = (.5-(index*.5)):max(0.01):min(0.5);
      allign = si.interpolate(index, 0.75, 0.5);
    };
    square(fund, index) = squareChooseP(fund, index, 0);
    squareP(fund, index) = squareChooseP(fund, index, 1);
    squareChooseP(fund, index, p) = (fnd(fund,allign,p)>=0.5), (ma.decimal((fnd(fund,allign,p)*2)+1)<:_-min(_,(-1*_+1)*((INDEX)/(1-INDEX)))) :+ *ma.PI:cos
    with {
      INDEX = index:max(ma.MIN):min(1-ma.MIN);
      allign = si.interpolate(INDEX, -0.25, 0);
    };

    pulse(fund, index) = pulseChooseP(fund, index, 0);
    pulseP(fund, index) = pulseChooseP(fund, index, 1);
    pulseChooseP(fund, index, p) = ((fnd(fund,allign,p)-min(fnd(fund,allign,p),((-1*fnd(fund,allign,p)+1)*(INDEX/(1-INDEX)))))*2*ma.PI):cos
    with {
      INDEX = index:min(0.99):max(0);
      allign = si.interpolate(index, -0.25, 0.0);
    };

    sinePulse(fund, index) = sinePulseChooseP(fund, index, 0);
    sinePulseP(fund, index) = sinePulseChooseP(fund, index, 1);
    sinePulseChooseP(fund, index, p) = (min(fnd(fund,allign,p)*((0.5-INDEX)/INDEX),(-1*fnd(fund,allign,p)+1)*((.5-INDEX)/(1-INDEX)))+fnd(fund,allign,p))*4*ma.PI:cos
    with {
      INDEX = ((index*-0.49)+0.5);
      allign = si.interpolate(index, -0.125, -0.25);
    };

    halfSine(fund, index) = halfSineChooseP(fund, index, 0);
    halfSineP(fund, index) = halfSineChooseP(fund, index, 1);
    halfSineChooseP(fund, index, p) = (select2(fnd(fund,allign,p)<.5, .5*(fnd(fund,allign,p)-.5)/INDEX+.5, fnd(fund,allign,p)):min(1))*2*ma.PI:cos
    with {
      INDEX = (.5-(index*0.5)):min(.5):max(.01);
      allign = si.interpolate(index:min(0.975), -0.25, -0.5);
    };
    fnd =
      case {
        (fund,allign,0) => fund;
        (fund,allign,1) => (fund+allign) : ma.frac; // allign phase with fund
      };
    resSaw(fund,res) = (((-1*(1-fund))*((cos((ma.decimal((max(1,res)*fund)+1))*2*ma.PI)*-.5)+.5))*2)+1;
    resTriangle(fund,res) = select2(fund<.5, 2-(fund*2), fund*2)*tmp*2-1
    with {
	  tmp = ((fund*(res:max(1)))+1:ma.decimal)*2*ma.PI:cos*.5+.5;
    };
    resTrap(fund, res) = (((1-fund)*2):min(1)*sin(ma.decimal(fund*(res:max(1)))*2*ma.PI));
  };

CZsaw(fund, index) = CZ.sawChooseP(fund, index, 0);
CZsawP(fund, index) = CZ.sawChooseP(fund, index, 1);
CZsquare(fund, index) = CZ.squareChooseP(fund, index, 0);
CZsquareP(fund, index) = CZ.squareChooseP(fund, index, 1);
CZpulse(fund, index) = CZ.pulseChooseP(fund, index, 0);
CZpulseP(fund, index) = CZ.pulseChooseP(fund, index, 1);
CZsinePulse(fund, index) = CZ.sinePulseChooseP(fund, index, 0);
CZsinePulseP(fund, index) = CZ.sinePulseChooseP(fund, index, 1);
CZhalfSine(fund, index) = CZ.halfSineChooseP(fund, index, 0);
CZhalfSineP(fund, index) = CZ.halfSineChooseP(fund, index, 1);
CZresSaw(fund,res) = CZ.resSaw(fund,res);
CZresTriangle(fund,res) = CZ.resTriangle(fund,res);
CZresTrap(fund, res) = CZ.resTrap(fund, res);

CZsawPO(fund, index,oct) = octaver(fund,CZsawP,index,oct);
CZsquarePO(fund, index,oct) = octaver(fund,CZsquareP,index,oct);
CZpulsePO(fund, index,oct) = octaver(fund,CZpulseP,index,oct);
CZsinePulsePO(fund, index,oct) = octaver(fund,CZsinePulseP,index,oct);
CZhalfSinePO(fund, index,oct) = octaver(fund,CZhalfSineP,index,oct);
CZresSawO(fund,res,oct) = octaver(fund,CZresSaw,res,oct);
CZresTriangleO(fund,res,oct) = octaver(fund,CZresTriangle,res,oct);
CZresTrapO(fund, res,oct) = octaver(fund,CZresTrap,res,oct);


sinPOF(allpassLevel,ms20level,oberheimLevel,normFreq,Q,fund,index,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,sine,index,oct);
sine(fund,index) = (fund*2*ma.PI:sin);
// sine(fund,index) = (fund*2*ma.PI:sin),(index:!);
CZsawPOF(allpassLevel,ms20level,oberheimLevel,normFreq,Q,fund,index,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZsawP,index,oct);
CZsquarePOF(allpassLevel,ms20level,oberheimLevel,normFreq,Q,fund,index,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZsquareP,index,oct);
CZpulsePOF(allpassLevel,ms20level,oberheimLevel,normFreq,Q,fund,index,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZpulseP,index,oct);
CZsinePulsePOF(allpassLevel,ms20level,oberheimLevel,normFreq,Q,fund,index,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZsinePulseP,index,oct);
CZhalfSinePOF(allpassLevel,ms20level,oberheimLevel,normFreq,Q,fund,index,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZhalfSineP,index,oct);
CZresSawOF(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,res,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZresSaw,res,oct);
CZresTriangleOF(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,res,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZresTriangle,res,oct);
CZresTrapOF(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,res,oct) = octaverFilter(fund,allpassLevel,ms20level,oberheimLevel,normFreq,Q,CZresTrap,res,oct);


sinPPF(f0,f1,oct,index) = oscPPF(f0,f1,index,oct,sine);
CZsawPPF(f0,f1,oct,index) = oscPPF(f0,f1,index,oct,CZsawP);
CZsquarePPF(f0,f1,oct,index) = oscPPF(f0,f1,index,oct,CZsquareP);
CZpulsePPF(f0,f1,oct,index) = oscPPF(f0,f1,index,oct,CZpulseP);
CZsinePulsePPF(f0,f1,oct,index) = oscPPF(f0,f1,index,oct,CZsinePulseP);
CZhalfSinePPF(f0,f1,oct,index) = oscPPF(f0,f1,index,oct,CZhalfSineP);
CZresSawPF(f0,f1,oct,res) = oscPPF(f0,f1,res,oct,CZresSaw);
CZresTrianglePF(f0,f1,oct,res) = oscPPF(f0,f1,res,oct,CZresTriangle);
CZresTrapPF(f0,f1,oct,res) = oscPPF(f0,f1,res,oct,CZresTrap);

oscPPF(f0,f1,index,oct,oscil) = (((f0,index):oscil),((f1,index):oscil)):si.interpolate(oct);
///////////////////////////////////////////////////////////////////////////////
//                                oscs fom PR                                 //
///////////////////////////////////////////////////////////////////////////////

lf_sawpos_reset(freq,reset) = ma.frac * (reset == 0) ~ +(freq/ma.SR);

lf_sawpos_phase_reset(freq,phase,reset) = lf_sawpos_reset(freq,reset) +phase :ma.frac;
// lf_sawpos_phase_reset(freq,phase,reset) = (+(phase-phase') : ma.frac * (reset == 0)) ~ +(freq/ma.SR);

//////////////////////////////////////////////////////////////////////////////
//                                 constants                                 //
//////////////////////////////////////////////////////////////////////////////

nrEnvelopes = 4;

minOct = -4;
maxOct = 4;

// fast
// stepsize = 0.1;
// medium
stepsize = 0.01;
// smooth
// stepsize = 0.001;

nrNotes = notes(diagram);
notes(0) = 127; // nr of midi notes
notes(1) = 4; // for block diagram
// nrNotes = 42; // for looking at bargraphs

maxAttack = 10;
maxDecay = maxAttack;
maxRelease = maxAttack;

diagram = 0;
// diagram = 1;
