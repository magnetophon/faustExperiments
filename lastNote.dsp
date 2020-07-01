declare author "Bart Brouns";
declare license "AGPLv3";
declare name "lastNote";
declare options "[midi:on]";
import("stdfaust.lib");
///////////////////////////////////////////////////////////////////////////////
//           give the number of the last note played                         //
///////////////////////////////////////////////////////////////////////////////
phase = hslider("phase", 0, 0, 1, stepsize);
// traditional faust synth:
freq = midiGroup(hslider("freq",440,0,24000,0.0001));
gain = midiGroup(hslider("gain",0.5,0,1,stepsize));
gate = midiGroup(button("gate"));

// workaround for proper monophonic handling, increases compile-time massively and CPU usage with +/- 5%:
// freq = lastNote:ba.pianokey2hz;
// gain = (vel(lastNote)/127); // increases the cpu-usage, from 7% to 11%
// gain = (nrNotesPlaying>0) // no velocity
// gate = vel(lastNote)>0;

// TODO: why this doesn't work,only gate works.
// from https://github.com/timowest/analogue/blob/master/faust/midi.dsp
// freq = hslider("/h:midi/pitch", 64, 32, 100, 1):ba.pianokey2hz;
// gain = hslider("/h:midi/gain", 1, 0, 1, 0.01);
// gate = button("/h:midi/gate");

///////////////////////////////////////////////////////////////////////////////
//                                  process                                  //
///////////////////////////////////////////////////////////////////////////////

process =
  CZsynth;
// (master,oscillatorIndex):CZsaw;
// master<:
// (
// sawGroup ((
// CZsaw(_, offset(oscillatorIndex,i))
// , offset(oscillatorLevel,i)))
// , pulseGroup ((
// CZpulse(_, offset(oscillatorIndex,i))
// , offset(oscillatorLevel,i)))
// );
// i=0;
// lf_sawpos_phase_reset(freq,phase,gate:ba.impulsify)*gate;

///////////////////////////////////////////////////////////////////////////////
//                                    GUI                                    //
///////////////////////////////////////////////////////////////////////////////

//groups///////////////////////////////////////////////////////////////////////

tabs(x) = tgroup("CZsynth", x);
synthGroup(x) = tabs(vgroup("[00]parameters", x));
midiGroup(x) = tabs(vgroup("[99]midi", x));
mainGroup(x) = vgroup("[0]main", x);
offsetGroup(x) = vgroup("[1]L-R offset", x);
sawGroup(x) = synthGroup(hgroup("[0]saw", x));
squareGroup(x) = synthGroup(hgroup("[1]square", x));
pulseGroup(x) = synthGroup(hgroup("[2]pulse", x));
sinePulseGroup(x) = synthGroup(hgroup("[3]sinePulse", x));
halfSineGroup(x) = synthGroup(hgroup("[4]halfSine", x));
resSawGroup(x) = synthGroup(hgroup("[5]resSaw", x));
resTriangleGroup(x) = synthGroup(hgroup("[6]resTriangle", x));
resTrapGroup(x) = synthGroup(hgroup("[7]resTrap", x));
//sliders//////////////////////////////////////////////////////////////////////
masterPhase = hslider("masterPhase", 0, -1, 1, stepsize) :new_smooth(0.999);

oscillatorIndex = hslider("index", 0, 0, 1, stepsize);
oscillatorRes = hslider("res", 0, 0, 64, stepsize);
oscillatorLevel = hslider("Level", 0, -1, 1, stepsize);
// sawIndex = sawGroup(oscillatorIndex);
// pulseIndex = pulseGroup(oscillatorIndex);

lfo_amount = hslider("lfo amount", 0, 0, 1, stepsize):new_smooth(0.999);
velocity(i) = midiGroup(select2(i>=0, 0, hslider("velocity of note %i [midi:key %i ]", 0, 0, nrNotes, 1)));

// pre-filter /////////////////////////////////////////////////////////////////
LPfreq = hslider("LPfreq[scale:log]", 24000, 20, 24000, 1);
normFreq = hslider("normFreq", 1, 0, 1, stepsize);
Q = hslider("Q", 1, stepsize, 10, stepsize);
filterBPlevel = hslider("filterBPlevel", 1, 0, 1, stepsize);
allpassLevel = hslider("allpassLevel", 0, 0, 1, stepsize);
ms20level = hslider("ms20level", 0, 0, 1, stepsize);
oberheimLevel = hslider("oberheimLevel", 0, 0, 1, stepsize);
///////////////////////////////////////////////////////////////////////////////
//                               implementation                              //
///////////////////////////////////////////////////////////////////////////////

CZsynth = par(i, 2, CZsynthMono(i));

CZsynthMono(i) =
  oscillators(i,master) : filters(i) : envelope(i)
// ,(master*gate)
// ,gain
// ,gate
;
oscillators(i,fund) =
  fund
// :fi.lowpass(LPfreq)
  :preFilter
   <:
   (
     sawGroup  ((
                 (_, offset(oscillatorIndex,i)):CZsawP
               , offset(oscillatorLevel,i)))
   , squareGroup  ((
                    (_, offset(oscillatorIndex,i)):CZsquareP
                  , offset(oscillatorLevel,i)))
   , pulseGroup((
                 (_, offset(oscillatorIndex,i)):CZpulseP
               , offset(oscillatorLevel,i)))
   , sinePulseGroup((
                     (_, offset(oscillatorIndex,i)):CZsinePulseP
                   , offset(oscillatorLevel,i)))
   , halfSineGroup((
                    (_, offset(oscillatorIndex,i)):CZhalfSineP
                  , offset(oscillatorLevel,i)))
   , resSawGroup  ((
                    (_, offset(oscillatorRes,i)):os.CZresSaw
                  , offset(oscillatorLevel,i)))
   , resTriangleGroup  ((
                         (_, offset(oscillatorRes,i)):os.CZresTriangle
                       , offset(oscillatorLevel,i)))
   , resTrapGroup  ((
                     (_, offset(oscillatorRes,i)):os.CZresTrap
                   , offset(oscillatorLevel,i)))
   )
  :mixer(8,1,1) ;

filters(i) = _;
envelope(i) = _ * gain * gate;

// master = lf_sawpos_reset(freq,reset) ;
master = lf_sawpos_phase_reset(freq,masterPhase,reset) ;
reset = gate:ba.impulsify;

offset(param,i) = mainGroup(param)+(offsetGroup(param) * select2(i,1,-1)) :new_smooth(0.999);

preFilter =
  _<:
  (
    _ , filterBPlevel
    , fi.allpassnn(1,normFreq/2*ma.PI), allpassLevel
    , ve.korg35LPF(normFreq,Q), ms20level
    , ve.oberheimLPF(normFreq,Q), oberheimLevel
  )
  : zeroSumMixer(4,1,1);

zeroSumMixer(nrInChan,nrOutChan,nrSends) = mixer(nrInChan,nrOutChan,nrSends);


lfo = 0.5*(1+os.osc(0.5));
vel(x) =  par(i, nrNotes, velocity(i)*(i==x)):>_ ;

///////////////////////////////////////////////////////////////////////////////
//                                oscs fom PR                                 //
///////////////////////////////////////////////////////////////////////////////

lf_sawpos_reset(freq,reset) = ma.frac * (reset == 0) ~ +(freq/ma.SR);

lf_sawpos_phase_reset(freq,phase,reset) = lf_sawpos_reset(freq,reset) +phase :ma.frac;
// lf_sawpos_phase_reset(freq,phase,reset) = (+(phase-phase') : ma.frac * (reset == 0)) ~ +(freq/ma.SR);
///////////////////////////////////////////////////////////////////////////////
//                                still to PR:                               //
///////////////////////////////////////////////////////////////////////////////

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

chooseFromFixed(N,maxN,expression) = par(i, maxN, expression(i)*(i==N)):>_;

CZsaw(fund, index) = CZsawChooseP(fund, index, 0);
CZsawP(fund, index) = CZsawChooseP(fund, index, 1);
CZsawChooseP(fund, index, p) =
  (((fnd(p)*((.5-tmp)/tmp)),(-1*fnd(p)+1)*((.5-tmp)/(1-tmp))):min+fnd(p))*2*ma.PI:cos
with {
  fnd =
    case {
      (0) => fund;
      (1) => (fund+allign) : ma.frac; // allign phase with fund
    };
  allign = si.interpolate(index, 0.75, 0.5);
  tmp = (.5-(index*.5)):max(0.01):min(0.5);
};

CZsquare(fund, index) = CZsquareChooseP(fund, index, 0);
CZsquareP(fund, index) = CZsquareChooseP(fund, index, 1);
CZsquareChooseP(fund, index, p) = (fnd(p)>=0.5), (ma.decimal((fnd(p)*2)+1)<:_-min(_,(-1*_+1)*((INDEX)/(1-INDEX)))) :+ *ma.PI:cos
with {
  INDEX = (index:pow(0.25)):max(0):min(1);
  fnd =
    case {
      (0) => fund;
      (1) => (fund+allign) : ma.frac; // allign phase with fund
    };
  allign = si.interpolate(INDEX, -0.25, 0);
};

CZpulse(fund, index) = CZpulseChooseP(fund, index, 0);
CZpulseP(fund, index) = CZpulseChooseP(fund, index, 1);
CZpulseChooseP(fund, index, p) = ((fnd(p)-min(fnd(p),((-1*fnd(p)+1)*(INDEX/(1-INDEX)))))*2*ma.PI):cos
with {
  INDEX = index:min(0.99):max(0);
  fnd =
    case {
      (0) => fund;
      (1) => (fund+allign) : ma.frac; // allign phase with fund
    };
  allign = si.interpolate(index, -0.25, 0.0);
};

CZsinePulse(fund, index) = CZsinePulseChooseP(fund, index, 0);
CZsinePulseP(fund, index) = CZsinePulseChooseP(fund, index, 1);
CZsinePulseChooseP(fund, index, p) = (min(fnd(p)*((0.5-INDEX)/INDEX),(-1*fnd(p)+1)*((.5-INDEX)/(1-INDEX)))+fnd(p))*4*ma.PI:cos
with {
  INDEX = ((index*-0.49)+0.5);
  fnd =
    case {
      (0) => fund;
      (1) => (fund+allign) : ma.frac; // allign phase with fund
    };
  allign = si.interpolate(index, -0.125, -0.25);
};

CZhalfSine(fund, index) = CZhalfSineChooseP(fund, index, 0);
CZhalfSineP(fund, index) = CZhalfSineChooseP(fund, index, 1);
CZhalfSineChooseP(fund, index, p) = (select2(fnd(p)<.5, .5*(fnd(p)-.5)/INDEX+.5, fnd(p)):min(1))*2*ma.PI:cos
with {
  INDEX = (.5-(index*0.5)):min(.5):max(.01);
  fnd =
    case {
      (0) => fund;
      (1) => (fund+allign) : ma.frac; // allign phase with fund
    };
  allign = si.interpolate(index:min(0.975), -0.25, -0.5);
};

// https://github.com/grame-cncm/faustlibraries/pull/44#issuecomment-651245377
new_smooth(s) = si.smooth(s * ((1-1') < 1) );

///////////////////////////////////////////////////////////////////////////////
//                               mixers from PR                              //
///////////////////////////////////////////////////////////////////////////////
monoMixerChannel(nrSends) =
  (
    si.bus(nrSends)
  , (_<:si.bus(nrSends))
  )
  :ro.interleave(nrSends,2)
  :par(i,nrSends,_*_);


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
///////////////////////////////////////////////////////////////////////////////
//                                  lastNote                                 //
///////////////////////////////////////////////////////////////////////////////

// TODO: make the uniqueIfy function only affect the new notes
// also: find out if it's needed at all
nrNotesPlaying = 0: seq(i, nrNotes, noteIsOn(i),_:+);
noteIsOn(i) = velocity(i)>0;

nrNotes = 127; // nr of midi notes
// nrNotes = 32; // for looking at bargraphs
// nrNotes = 4; // for block diagram

lastNote =
  (
    par(i, nrNotes, i)
  , ((par(i, nrNotes, index(i)),uniqueIfy):ro.interleave(nrNotes,2):par(i, nrNotes, +))
  ):ro.interleave(nrNotes,2)
  :find_max_index(nrNotes):(_,!)
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
find_max_index(N) = seq(i,N-2, (index_comparator,si.bus(2*(N-i-2)))) : index_comparator;

uniqueIfy =
  (0:seq(i, nrNotes, myBus(i),(_-(noteIsOn(i-1)*(nrNewNotes>1))<:(_,_)) ):(si.bus(nrNotes),!));

// };
//////////////////////////////////////////////////////////////////////////////
//                                 constants                                 //
//////////////////////////////////////////////////////////////////////////////
// fast
// stepsize = 0.1;
// medium
stepsize = 0.01;
// smooth
// stepsize = 0.001;
