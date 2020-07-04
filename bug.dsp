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
freq = lastNote:ba.pianokey2hz : enabled_smooth(gate',ba.tau2pole(portamento));
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
os.osc(freq)*gain*gate;

velocity(i) = select2(i>=0, 0, hslider("velocity of note %i [midi:key %i ]", 0, 0, nrNotes, 1));
portamento = hslider("[0]portamento[scale:log]", 0, 0, 1, stepsize);
enabled_smooth(e,s) = si.smooth(s * e );
vel(x) = chooseFromFixed(nrNotes,velocity,x);
// vel(x) = x:chooseFromFixed(nrNotes,velocity);
// you need the value of `expression(N)`, but expression wants N to be known at compile time.
// if you know N will be an int between 0 and maxN, you can do:
chooseFromFixed(maxN,expression,N) = par(i, maxN, expression(i)*(i==N)):>_;
///////////////////////////////////////////////////////////////////////////////
//                                  lastNote                                 //
///////////////////////////////////////////////////////////////////////////////

nrNotesPlaying = 0: seq(i, nrNotes, noteIsOn(i),_:+);
noteIsOn(i) = velocity(i)>0;

lastNote =
  par(i, nrNotes, (i,index(i)) )
  : find_max_index(nrNotes)
  : (_,!)
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

//////////////////////////////////////////////////////////////////////////////
//                                 constants                                 //
//////////////////////////////////////////////////////////////////////////////
// fast
// stepsize = 0.1;
// medium
stepsize = 0.01;
// smooth
// stepsize = 0.001;

nrNotes = 127; // nr of midi notes
// nrNotes = 4; // for block diagram
