import("stdfaust.lib");

BufferLength=300;

pitch(x) =
  // i
  ma.SR / max(M, 1) - ma.SR * (M == 0)
with {
  AC(x, k) = sum(j, BufferLength, (x@(j))*(x@(j+k))) / (BufferLength) ; // Correlating two signals, on the buffer length, ideally over 600 samples with actual SR
  // i = +(1) ~_ ;
  i = +(1) ~_ :min(1e9):max(0);
  // i = min(1e8,+(1)) ~_ ;
  // i = min(1e3,+(1):max(0)) ~_ ;
  // i = min(1e8,+(1)) ~_ ;
  U = AC(x,i)  >  AC(x,0)/2;             // Detection of a good correlation, AC(x,0) being the highest possible value, so its half becomes the detection threshold
  V = AC(x,i)  <= AC(x,i+1);
  W = AC(x,i+1) > AC(x,i+2);
  Y = U: *(V) : *(W);

  N = (+(1) : *(1 - Y)) ~ _;
  M = (N' + 1) : ba.sAndH(Y);
};

// track min and max of vibrato amp
// track when we are inside vibrato
// track which state the current prediction is in: stable, vibing up, vibing down, sliding up, sliding down
// stable = slidingAvg over nrNoteSamples of prevStable;
// nrNoteSamples = counter that resets when we get a new note
//
//  get the 3 rust predictions, p0, p1, p2
//
minPred = min3(p0,p1,p2);
maxPred = max3(p0,p1,p2);
center = p0+p1+p2-minPred-maxPred;

// use 


// 
// 4 rwTables of all 128 notes:
// one adding the new to the prev val if the prediction is close to that note
// one counting how many times we've been close to this particuar note
// one that stores the avg, so adding/counting
// 
// (maybe beter tho make it weigted: the closer, the higher the weight)
// store timestamp of prediction 
//
//
// freq =
// case {
//   one or more of the predictions are close to the prev avg note, pick the closest
//   one or more of the predictions are close to the prev freq, pick the closest
//   one or more of of the predictions has a derivative that is close to prevDerivative, pick the closest (iow: we keep going in a similar direction)
//   all 3 are the same octave-jump relative to prev avg or freq: shift octave back and pick the closest to prev freq : ignore first octave jump
//   all 3 are the same octave-jump relative to prev avg or freq and close to center of previous predictions: pick the center: don't ignore second octave jump
//   all 3 are close to center of previous predictions: pick the center // maybe this case makes the one above redundant, but maybe both need different definitions of "close"
//   all 3 are close to the same transposition of avg note, pick the one closest to the transposition
//   all 3 are close to the same transposition of freq, pick the one closest to the transposition
//   
//   two predictions are close together and the other is an octave of that, shift the octave and select which of the 3 is closest to the prev freq and/or avg
//   two predictions are close together and the other is a transposition of that, shift the transposition and select which of the 3 is closest to the prev freq
//
//   
//   
//   
//   two predictions are in the same direction as the prevDerivative choose the one that is closest to the prevDerivative
//   close to one of the  of the prev note's octaves, count how often that happens and transition if often enough
//   close to one of the  of the prev note's transpositions, weigh with frecency and closeness
// }
//
// for the octave and transition counting, we only take the predictions if in that same timestamp there where no predictions close to the avg Note or prev freq
// close to can mean either less than 0.5 semitone away, or less than (max(maxVibratoAmp, abs(minVibratoAmp)))

// weigting system:
// count how many times this note has been close in the recent guesses
// more recent: more weight
// close to prevWeightedAVG for this note: more weight
// 
// table 128 added weights 
// table 128 added freqs 
// 
// 
// make 3 prediction branches: stable, up, down
// or: make 5 prediction branches: stable, up, down, octave up & down
// or: make 25 prediction branches: stable, 12 up, 12 down


process = pitch : hbargraph("Frequency", 50, 2000): os.sawtooth ;
// can we do this whole turnaround thing also for a limiter?

// States are:
// onNote
// vibrato 
// on
// (maybe later onPythagoran and onEqualTemperament)
fbPitch(prevPitch, prevNote, prevCents, prevMaxVibratoAmp, prevA440  ) =
  Pitch
, Note // distance to previous note from (0-notesInOctave) to notesInOctave float, relative to current note
  // maybe better expressed as position within the octave from 0 to 1, relative to the current
, maxVibratoAmp // largest of the amplitudes of the last turnaround points, where the pitch came back to it's center 
, minVibratoAmp // smallest of the amplitudes of the last turnaround points, where the pitch came back to it's center 
  // , A440
  // in a later stage, tuningSytem & scale, for now assume equal temperament and all 12 notes
  // , tuningSytem // one of:
  // equal temprament
  // pythagorean
  // (maybe later more, like tet24 etc)
  // auto:keep track of which one was most common, with frecency
  //
  // , scale // a table of our current best guess as to what the 12 places in the octave are
  // implemented as 
  // 
with {
  notesInOctave = 12; 
  maxVibratoAmp =
    select2(isNewNote
           , max(vibratoAmp, prevMaxVibratoAmp) // vibratoAmp could be more than +/- 0.5 note, but only if
           , 0 // the amp relative to the new note
           );
  // isNewNote = prevCents < 100 && Cents >= 100 &&  ;
  isNewNote =
    (Note <= 1 && prevNote >= 1) // if we came back from (or stayed on) a slide of more than one note up, it must be a new note
    || (Note >= -1 && prevNote <= 1) // same for if it's the note below
    || (Note <= notesInOctave && prevNote >= notesInOctave ) // if we came back from (or stayed on) a slide of more than an octave up, it must also be a new note
    || (Note >= (0-notesInOctave) && prevNote <= (0-notesInOctave) ); // same for if it's the octave below
  // added advatage: this way we automatically ignore the first wobble after the slide
  // should also help with octave errors: if it's just a single error guess, it only takes the 
  

  Note = inCorrectnessFactor * prevNote + (1-inCorrectnessFactor ) * 0;
  // interpolate_linear(dv,v0,v1) = v0 + dv*(v1-v0);  // (faster than v0*(1-dv)+v1*dv which is currently not optimized...)
  // Note = it.interpolate_linear(inCorrectnessFactor,0,prevNote);
  // Note = inCorrectnessFactor*prevNote;
  NoteSpeed = it.interpolate_linear(inCorrectnessFactor,speedAtZero,speedAtPrevNote);
  // alternative: LUT with different speeds for different notes:
  // or a live updated table with a weight for each note in the oscatave, with 0 meaning: this is the only note I've ever seen 
  Note = fi.SVFTPT.LP2(NoteSpeed, .5, newGuess);
  inCorrectnessFactor = min(1, abs(prevNote)); // maybe better: softSat( abs(prevNote) )
  // or a triangle with min @ note when 

}
