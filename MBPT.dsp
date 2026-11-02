declare name "MBPT";
declare version "0.4";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026 - 2026, Bart Brouns";

import("stdfaust.lib");

// A monophoonic multi band pitch travker.
//
//Working principle:

// - a bandpass around each note of interest
// - get the freq using zero crossings, get the level using an envelope follower
// - group by pitchclass and sum the levels per pitchclass
// - pick the loudest pitchclass.
// - pick the loudest nrSynthesisBands of the chosen pitchclass and its fifth
//   (the fifth's levels are scaled by "fifth weight", just like in the pitchclass sum)
// - synthesize them: one sine per band, summed to mono
//
// MBPT output: nrSynthesisBands pairs of (freq, level), loudest first.
// process output: the sum of the sines, mono.

// can be at most nrCandidates (see MBPT):
nrSynthesisBands = 8;

process = // pitchTracker(1, 0.1);
MBPT:synthesis;

//////////////////////////////////////////////////////////////////////////
// synthesis
//////////////////////////////////////////////////////////////////////////

// nrSynthesisBands * (freq, level) -> mono
synthesis = par(i, nrSynthesisBands, sineVoice):>_:*(volume)
    with {
        sineVoice(f, l) = os.osc(f)*l;
        volume = hslider("synth volume [unit:dB]", -6, -60, 12, 0.1):ba.db2linear:si.smoo;
    };

//////////////////////////////////////////////////////////////////////////
// analysis
//////////////////////////////////////////////////////////////////////////

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
        // frequency from the time between successive rising zero crossings, with the
        // crossing times linearly interpolated to sub-sample precision. Held until the next
        // crossing. (The old an.zcr(1/CF(i), x)*ma.SR*.5 lowpassed the crossing pulses with
        // a time constant of only one period, which left about +-25% ripple on the pitch.)
        analysisFreq(i, x) = (held(x)>0)*ma.SR/max(1, held(x));
        rise(x) = (x>0)&(x'<=0);
        a(x) = max(0, min(1, (0-x')/max(1e-30, x-x')));
        c(x) = loop~_
            with {
                loop(prev) = (prev+1)*(1-rise(x));
            };
        aHeld(x) = ba.latch(rise(x), a(x));
        period(x) = (c(x)'+1)+a(x)-aHeld(x)';
        held(x) = ba.latch(rise(x), period(x)*(aHeld(x)'>=0)*(c(x)'>0));
        // level(i, x) = an.amp_follower(2/CF(i), x);
        level(i, x) = an.amp_follower_ar(1/CF(i), 2/CF(i), x);
        // level(i, x) = si.smooth(ba.tau2pole(2/CF(i)), x);
        CF(i) = ba.pianokey2hz(i+baseFreqNr);
        // from Ab0, 25.956543 Hz:
        baseFreqNr = 0;
        // to G8, 6271.927 Hz:
        nrAnalysisOctaves = 8;
        nrAnalysisBands = nrAnalysisOctaves*12;
        // candidates = the chosen pitchclass + its fifth, nrAnalysisOctaves bands each.
        // Must be a power of two (bitonic sort).
        nrCandidates = 2*nrAnalysisOctaves;
        Q = hslider("Q", 0.7, 0.01, 17, 0.01);

        argmax12 = (si.bus(12), par(i, 4, -1)):ro.bitonicSortIdx(16):(par(i, 15, !), _);

        fifthWeight = hslider("fifth weight", 0.4, 0, 1, 0.01);

        // output i = input (i+7)%12 ; route indices are 1-based
        rot7 = route(12, 12, par(i, 12, ((i+7)%12+1, i+1)));

        withFifth = si.bus(12)<:(si.bus(12), (rot7:par(i, 12, *(fifthWeight)))):>si.bus(12);

        // bands are numbered band = octave*12 + pitchclass
        chooseLoudestPitchclass = // split into [96 freqs, 96 levels]:
        ro.interleave(2, nrAnalysisBands)// freqs pass through; levels pass through and also get summed per pitchclass (+fifths):
        :(si.bus(nrAnalysisBands), (si.bus(nrAnalysisBands)<:(si.bus(nrAnalysisBands), pitchclassLevels)))// freqs and levels pass through, argmax gives the loudest pitchclass:
        :(si.bus(nrAnalysisBands), si.bus(nrAnalysisBands), argmax12)// pick the loudest bands of that pitchclass and its fifth:
        :pickLoudestBands;

        // add the levels, also for the fifths:
        // group the levels by pitchclass (8 octaves per class), sum them, add the fifths:
        pitchclassLevels = ro.interleave(12, nrAnalysisOctaves):par(i, 12, si.bus(nrAnalysisOctaves):>_):withFifth;

        //////////////////////////////////////////////////////////////////////////
        // picking the loudest bands
        //////////////////////////////////////////////////////////////////////////

        // [freqs(96), levels(96), pitchclass] -> nrSynthesisBands * (freq, level), loudest first
        pickLoudestBands = // add the pitchclass of the fifth:
        (si.bus(2*nrAnalysisBands), (_<:(_, fifthOf)))// -> [freqs(16), levels(16)] of the pitchclass' bands, then those of the fifth:
        :gatherCandidates// keep freqs and levels, and sort a copy of the levels to get the ranking (ascending index list):
        :(si.bus(2*nrCandidates)<:(si.bus(2*nrCandidates), (par(i, nrCandidates, !), si.bus(nrCandidates)))):(si.bus(2*nrCandidates), (ro.bitonicSortIdx(nrCandidates):loudestFirst))// use the ranking to select the freqs and levels:
        :selectRanked;

        fifthOf = int:+(7):%(12);

        // the n loudest indices, loudest first:
        loudestFirst = par(i, nrCandidates-nrSynthesisBands, !), si.bus(nrSynthesisBands):ro.cross(nrSynthesisBands);

        // wired selector: (index, x0 .. x(N-1)) -> x(index), no signal-expression needed:
        selectW(N) = ((_<:par(j, N, ==(j))), si.bus(N)):ro.interleave(N, 2):par(j, N, *):>_;

        // [freqs(96), levels(96), pitchclass, fifth pitchclass] -> [freqs(16), levels(16)]
        // candidate c < nrAnalysisOctaves: octave c of the pitchclass
        // candidate c >= nrAnalysisOctaves: octave c-nrAnalysisOctaves of the fifth
        gatherCandidates = route(2*nrAnalysisBands+2, groupSize*2*nrCandidates, par(g, 2*nrCandidates, (selSrc(g), g*groupSize+1), par(j, 12, (bandSrc(g, j), g*groupSize+j+2)))):par(g, 2*nrCandidates, selectW(12))
            with {
                groupSize = 13;
                // g < nrCandidates: freq of candidate g; else: level of candidate g-nrCandidates
                kind(g) = int(g/nrCandidates);
                cand(g) = g%nrCandidates;
                isFifth(g) = int(cand(g)/nrAnalysisOctaves);
                oct(g) = cand(g)%nrAnalysisOctaves;
                // 1-based input numbers:
                selSrc(g) = 2*nrAnalysisBands+1+isFifth(g);
                bandSrc(g, j) = kind(g)*nrAnalysisBands+oct(g)*12+j+1;
            };

        // [freqs(16), levels(16), index of the n loudest (loudest first)] -> n * (freq, level)
        selectRanked = route(2*nrCandidates+nrSynthesisBands, groupSize*2*nrSynthesisBands, par(k, nrSynthesisBands, par(kind, 2, (2*nrCandidates+k+1, (2*k+kind)*groupSize+1), par(c, nrCandidates, (kind*nrCandidates+c+1, (2*k+kind)*groupSize+c+2))))):par(g, 2*nrSynthesisBands, selectW(nrCandidates))
            with {
                groupSize = nrCandidates+1;
            };
    };
