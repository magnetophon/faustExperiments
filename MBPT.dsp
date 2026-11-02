declare name "MBPT";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026 - 2026, Bart Brouns";

import("stdfaust.lib");

process = // pitchTracker(1, 0.1);
MBPT;

MBPT(x) = //
x<:par(i, nrAnalysisBands, oneBandPT(i)):chooseLoudest//
// par(i, nrAnalysisBands, CF(i))
    with {
        oneBandPT(i, x) = fi.SVFTPT.BP2Norm(CF(i), Q, x)<:(analysisFreq(i), level(i));
        analysisFreq(i, x) = an.zcr(1/CF(i), x)*ma.SR*.5;
        level(i, x) = an.amp_follower(2/CF(i), x);
        CF(i) = ba.pianokey2hz(i+baseFreqNr);
        // from Ab0, 25.956543 Hz:
        baseFreqNr = 0;
        // to G8, 6271.927 Hz:
        nrAnalysisOctaves = 8;
        nrAnalysisBands = nrAnalysisOctaves*12;
        // nrAnalysisBands = 96;
        Q = hslider("Q", 0.7, 0.01, 13, 0.01);

        chooseLoudest = ro.interleave(2, nrAnalysisBands)//
        :par(i, 2, ro.interleave(nrAnalysisOctaves, 12));
    };
