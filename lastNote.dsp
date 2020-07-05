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
// freq = midiGroup(hslider("freq",440,0,24000,0.0001)) :new_smooth( ba.tau2pole(portamento));
// freq = midiGroup(hslider("freq",440,0,24000,0.0001)) :si.smooth(gate' * ba.tau2pole(portamento));
// gain = midiGroup(hslider("gain",0.5,0,1,stepsize));
// gate = midiGroup(button("gate"));

// workaround for proper monophonic handling, increases compile-time massively and CPU usage with +/- 5%:
// freq = lastNote:ba.pianokey2hz :new_smooth(ba.tau2pole(portamento));
// freq = lastNote:ba.pianokey2hz :si.smooth(gate' * ba.tau2pole(portamento));
freq = lastNote:ba.pianokey2hz : enabled_smooth(gate' & gate , ba.tau2pole(portamento));
// freq = lastNote:ba.pianokey2hz :smoothDelayedBy(2,ba.tau2pole(portamento));
gain = (vel(lastNote)/127); // increases the cpu-usage, from 7% to 11%
gate = gain>0;
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
  // no.noise*0.5<:(ve.oberheimLPF(normFreq,Q),fi.lowpass(4,LPfreq));
  // oneSumMonoMixerChannel(3);
  // oneSumMixer(4,3,2);
  // oneSumMixer(3,2,1);
  FallbackMixer(5,3,2,(si.bus(3)));
// CZsynth;

///////////////////////////////////////////////////////////////////////////////
//                                    GUI                                    //
///////////////////////////////////////////////////////////////////////////////

//groups///////////////////////////////////////////////////////////////////////

tabs(x) = tgroup("CZsynth", x);
oscillatorGroup(x) = tabs(vgroup("[00]oscillators", x));
// envelopeGroup(x) = tabs(vgroup("[01]envelope", x));
envelopeGroup(i,x) = tabs(vgroup("[%i]envelope %i", x));
midiGroup(x) = tabs(vgroup("[99]midi", x));
mainGroup(x) = vgroup("[0]main", x);
offsetGroup(x) = vgroup("[1]L-R offset", x);
sineGroup(x)        = oscillatorGroup(hgroup("[0]sine", x));
sawGroup(x)         = oscillatorGroup(hgroup("[1]saw", x));
squareGroup(x)      = oscillatorGroup(hgroup("[2]square", x));
pulseGroup(x)       = oscillatorGroup(hgroup("[3]pulse", x));
sinePulseGroup(x)   = oscillatorGroup(hgroup("[4]sinePulse", x));
halfSineGroup(x)    = oscillatorGroup(hgroup("[5]halfSine", x));
resSawGroup(x)      = oscillatorGroup(hgroup("[6]resSaw", x));
resTriangleGroup(x) = oscillatorGroup(hgroup("[7]resTriangle", x));
resTrapGroup(x)     = oscillatorGroup(hgroup("[8]resTrap", x));
//sliders//////////////////////////////////////////////////////////////////////
masterPhase = hslider("masterPhase", 0, -1, 1, stepsize) :new_smooth(0.999);
portamento = hslider("portamento[scale:log]", 0, 0, 1, stepsize);

oscillatorIndex = hslider("index", 0, 0, 1, stepsize);
oscillatorRes   = hslider("res", 0, 0, 64, stepsize);
oscillatorLevel = hslider("Level", 0, -1, 1, stepsize);
// sawIndex = sawGroup(oscillatorIndex);
// pulseIndex = pulseGroup(oscillatorIndex);
attack(i)  = envelopeGroup(i,hslider("[0]attack", 0, 0, 1, stepsize));
decay(i)   = envelopeGroup(i,hslider("[1]decay", 0.1, 0, 1, stepsize));
sustain(i) = envelopeGroup(i,hslider("[2]sustain", 0.8, 0, 1, stepsize));
release(i) = envelopeGroup(i,hslider("[3]release", 0.1, 0, 1, stepsize));


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
smoothDelayedBy(d, s) = *(1.0 - sd) : + ~ *(sd)
with {
  sd = s * ((1-1@d) < 1) ;
};
CZsynth = par(i, 2, CZsynthMono(i));

CZsynthMono(i) =
  oscillators(i,master) : filters(i) : envelope(0)
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
        sineGroup  ((
                     offset(oscillatorLevel,i)
                   , (_*2*ma.PI:sin)
        ))

      , sawGroup  ((
                    offset(oscillatorLevel,i)
                      , ((_, offset(oscillatorIndex,i)):CZsawP)
            ))
      , squareGroup  ((
                     offset(oscillatorLevel,i)
                     ,((_, offset(oscillatorIndex,i)):CZsquareP)
      ))
      , pulseGroup((
                  offset(oscillatorLevel,i)
                  , ((_, offset(oscillatorIndex,i)):CZpulseP)
      ))
      , sinePulseGroup((
                      offset(oscillatorLevel,i)
                      , ((_, offset(oscillatorIndex,i)):CZsinePulseP)
      ))
      , halfSineGroup((
                       offset(oscillatorLevel,i)
                     , ((_, offset(oscillatorIndex,i)):CZhalfSineP)
      ))
      , resSawGroup  ((
                       offset(oscillatorLevel,i)
                     , ((_, offset(oscillatorRes,i)):CZresSaw)
      ))
      , resTriangleGroup  ((
                            offset(oscillatorLevel,i)
                          , ((_, offset(oscillatorRes,i)):CZresTriangle)
      ))
      , resTrapGroup  ((
                        offset(oscillatorLevel,i)
                      , ((_, offset(oscillatorRes,i)):CZresTrap)
      ))
      )
     :oneSumMixer(9,1,1) ;

   filters(i) = _;
   envelope(i) =  _*adsreg(attack(i),decay(i),sustain(i),release(i),gate,gain);
   // envelope(i) =  _*adsre(attack(i),decay(i),sustain(i),release(i),gate);
   // envelope(i) = _*en.adsre(attack(i),decay(i),sustain(i),release(i),gate);

   // master = lf_sawpos_reset(freq,reset) ;
   master = lf_sawpos_phase_reset(freq,masterPhase,reset) ;
   reset = gate:ba.impulsify;

   offset(param,i) = mainGroup(param)+(offsetGroup(param) * select2(i,1,-1)) :new_smooth(0.999);

   preFilter =
     _<:
     (
       filterBPlevel , _
       , allpassLevel , fi.allpassnn(1,normFreq/2*ma.PI)
       , ms20level , ve.korg35LPF(normFreq,Q)
       , oberheimLevel , ve.oberheimLPF(normFreq,Q)
     )
     : oneSumMixer(4,1,1);

   // oneSumMixer(nrInChan,nrOutChan,nrSends) =
   // par(i, nrInChan, si.bus(nrSends),si.bus(nrOutChan)) : mixer(nrInChan,nrOutChan,nrSends) : par(i, nrSends, si.bus(nrOutChan));


   lfo = 0.5*(1+os.osc(0.5));
   // vel(x) = chooseFromFixed(nrNotes,velocity,x);
   vel(x) = x:chooseFromFixed(nrNotes,velocity);
   //par(i, nrNotes, velocity(i)*(i==x)):>_ ;

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
   FallbackMixer(nrInChan,nrOutChan,nrSends,fallBack) =
     (
       (ro.interleave(nrOutChan+nrSends,nrInChan)
        : (FallbackGains
          , ro.interleave(nrInChan, nrOutChan)
        ))
     , fallBack
     )
:
(si.bus((nrInChan+1)*nrSends), ro.interleave(nrOutChan,nrInChan+1) )
: ro.interleave(nrInChan+1,nrOutChan+nrSends)
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
     :find_max_index(nrNotes):(_,!)
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
   find_max_index(N) = seq(i,N-2, (index_comparator,si.bus(2*(N-i-2)))) : index_comparator;

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
         INDEX = (index:pow(0.25)):max(0):min(1);
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

   ///////////////////////////////////////////////////////////////////////////////
   //                                oscs fom PR                                 //
   ///////////////////////////////////////////////////////////////////////////////

   lf_sawpos_reset(freq,reset) = ma.frac * (reset == 0) ~ +(freq/ma.SR);

   lf_sawpos_phase_reset(freq,phase,reset) = lf_sawpos_reset(freq,reset) +phase :ma.frac;
   // lf_sawpos_phase_reset(freq,phase,reset) = (+(phase-phase') : ma.frac * (reset == 0)) ~ +(freq/ma.SR);

   //////////////////////////////////////////////////////////////////////////////
   //                                 constants                                 //
   //////////////////////////////////////////////////////////////////////////////
   // fast
   // stepsize = 0.1;
   // medium
   stepsize = 0.01;
   // smooth
   // stepsize = 0.001;

   // nrNotes = 127; // nr of midi notes
   nrNotes = 42; // for looking at bargraphs
   // nrNotes = 4; // for block diagram
