declare name "MBPT";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026 - 2026, Bart Brouns";

import("stdfaust.lib");

process = // pitchTracker(1, 0.1);
MBPT;

MBPT(x) = //
x<:par(i, nrAnalysisBands, oneBandPT(i)):chooseLoudestPitchclass//
// par(i, nrAnalysisBands, CF(i))
    with {
        // fi.bandpass(2 seems to give the best ring/ejection compromise 
        // sqrt(t) puts the band edges symmetrically around CF on a log-frequency axis,
        // so the filter is exactly bw semitones wide with CF in the geometric middle.
        oneBandPT(i, x) = fi.bandpass(2, CF(i)/sqrt(r), CF(i)*sqrt(r), x)<:(analysisFreq(i), level(i));
        r = 2^(bw/12);
        bw = hslider("bandwidth [unit:semitones]", 1, 0.25, 6, 0.01);
        analysisFreq(i, x) = an.zcr(1/CF(i), x)*ma.SR*.5;
        level(i, x) = an.amp_follower(2/CF(i), x);
        CF(i) = ba.pianokey2hz(i+baseFreqNr);
        // from Ab0, 25.956543 Hz:
        baseFreqNr = 0;
        // to G8, 6271.927 Hz:
        nrAnalysisOctaves = 8;
        nrAnalysisBands = nrAnalysisOctaves*12;
        Q = hslider("Q", 0.7, 0.01, 17, 0.01);

        argmax12 = (si.bus(12), par(i, 4, -1)):ro.bitonicSortIdx(16):(par(i, 15, !), _);

        chooseLoudestPitchclass = //
        // sort to freqs and levels:
        ro.interleave(2, nrAnalysisBands)//
        // sort in pitchclasses
        :par(i, 2, ro.interleave(12, nrAnalysisOctaves))//
        // pass through the pitches
        :(si.bus(nrAnalysisBands), //
        // add the levels:
        par(i, 12, si.bus(nrAnalysisOctaves):>_))//
        // pass through the pitches
        :(si.bus(nrAnalysisBands), //
        // pass through the levels and give the index of the loudest oitchclass:
        (si.bus(12)<:(si.bus(12), argmax12)));
    };
