declare name "subsynth";
declare version "0.3";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2026, Bart Brouns";
declare description "Monophonic sub-bass synth with a pitch-invariant spectrum: partials are shaped by absolute frequency only and fade in Shepard-style over the octave above the speaker limit, so any note gives the same spectral envelope and octave wraps are inaudible. Peak auto-normalized to 0 dBFS at velocity 127.";
declare options "[midi:on]";
import("stdfaust.lib");

//========================= compile-time constants =========================
N = 16;          // harmonics synthesized (m = 1..N). For a seamless wrap the series must reach the
                 // ceiling from the bottom of the octave: N * lo >= hi * 2^edgeOct, i.e. N >= 1.26 * hi/lo.
M = 512;         // phase points per cycle used to measure the peak (see "auto gain" below)
tiltRange = 4;   // low/high balance at +-1 = +-24 dB/oct across the band
fadeOct = 1;     // width, in octaves, of the fade-in above the speaker limit. 1 = ideal Shepard blend
                 // (most gradual); smaller widens the full-level band but makes wraps more audible.
edgeOct = 1/3;   // width, in octaves, of the raised-cosine fade-out above the bass ceiling
smoothTau  = 0.005;  // parameter smoothing time constant (s)
attackTau  = 0.005;  // gate attack time constant (s)
releaseTau = 0.03;   // gate release time constant (s)

//========================= UI =========================
lo     = hslider("[0]speaker low limit [unit:Hz][scale:log][tooltip:lowest freq the speaker can handle. Nothing sounds below it; partials fade in over the octave above it]", 30, 10, 200, 0.1);
hi     = hslider("[1]bass ceiling [unit:Hz][scale:log][tooltip:highest freq we still consider bass. Partials above it fade out]", 120, 40, 1000, 0.1);
family = hslider("[2]overtones/octaves balance [tooltip:-1 = only the odd overtones (3rd, 5th, 7th... and their octaves), +1 = only the fundamental and its octaves]", 0, -1, 1, 0.01);
tilt   = hslider("[3]low/high balance [tooltip:spectral tilt across the band, -1 = -24 dB/oct, +1 = +24 dB/oct]", -0.5, -1, 1, 0.01);
// manual play, for testing without MIDI; overridden while a MIDI key is held
freqSlider = hslider("[4]freq [unit:Hz]", 55, 8, 20000, 0.001);
gainSlider = hslider("[5]gain", 1, 0, 1, 0.001);
gateButton = button("[6]gate");

//========================= MIDI: monophonic, low-note priority =========================
// Tracked inside the DSP rather than with the polyphonic voice manager (-nvoices 1), which
// only computes a voice after a MIDI note-on and so leaves the GUI gate dead. Each key writes
// its velocity into a hidden slider on note-on and 0 on note-off; the lowest held key plays.
// All of this is control-rate, so the peak measurement below stays once-per-block.
key(i)    = hslider("[9]k%i [midi:key %i][hidden:1]", 0, 0, 127, 1);
lowestKey = par(i, 128, select2(key(i) > 0, 128, i)) : ba.parallelMin(128);   // 128 = nothing held
midiGate  = lowestKey < 128;
midiVel   = par(i, 128, key(i) * (i == lowestKey)) :> _;
noteFreq  = select2(midiGate, freqSlider, ba.midikey2hz(lowestKey));
velocity  = select2(midiGate, gainSlider, midiVel / 127);
gate      = max(gateButton, midiGate);

//========================= note folding =========================
// The note is transposed by whole octaves into [lo, 2*lo). Which octave is only a labelling
// choice: the weights below depend on absolute frequency and octave family alone, so the
// partial set of a note and of the same note an octave up coincide wherever both are audible.
f0 = fold(max(noteFreq, 1e-3));
fold(f) = f / pow(2, floor(ma.log2(f / lo) + 1e-6));   // 1e-6 oct: a note exactly on lo stays put

//========================= per-partial weights (control-rate) =========================
// Partial m = q * 2^v with q odd: q = 1 are the fundamental and its octaves, q >= 3 the odd
// overtones and theirs. Its weight is
//   family(q) * fadeIn(m*f0 / q) * tilt(m*f0) * ceiling(m*f0)
// fadeIn takes the frequency of the partial's octave family (2^v * f0), so all families fade
// in over the same octave [lo, 2*lo): the family that appears from silence when the note wraps
// is exactly the one that reached full level an octave up. This is the Shepard-tone construction.
v2(m)       = sum(j, 8, (m % (1 << (j + 1))) == 0);    // 2-adic valuation of m (m < 512)
oddPart(m)  = m / (1 << v2(m));
familyW(m)  = select2(oddPart(m) == 1, cos(a), sin(a))   // equal-power crossfade overtones <-> octaves
with { a = 0.25 * ma.PI * (family + 1); };
fadeIn(f)   = pow(sin(0.5 * ma.PI * x), 2) with { x = ma.log2(f / lo) / fadeOct : max(0) : min(1); };
tiltW(f)    = pow(f / lo, tilt * tiltRange);
ceiling(f)  = pow(cos(0.5 * ma.PI * x), 2) with { x = ma.log2(f / hi) / edgeOct : max(0) : min(1); };
weight(m)   = familyW(m) * fadeIn(m * f0 / oddPart(m)) * tiltW(m * f0) * ceiling(m * f0);

//========================= waveform =========================
// harmonics in sine phase. N harmonic gains in, one sample out. The gains are passed as a
// bus, not as a closure: a closure would be re-expanded by the box evaluator (memoized per
// environment) for every one of the N*M/2 probe terms below.
wave(ph) = par(k, N, *(sin(2 * ma.PI * (k + 1) * ph))) :> _;
gains    = par(k, N, weight(k + 1));

//========================= auto gain =========================
// The waveform is periodic and depends only on control-rate gains, so its peak is measured
// once per block by evaluating one cycle on M phase points: no latency and no audio-rate
// cost (the sines fold to constants, leaving N*M/2 multiply-adds per block).
// Sine phases make the waveform odd, so |wave| over half a cycle covers the whole cycle;
// probe the full cycle instead if you ever add cosine partials.
// A trig polynomial of degree N sampled on M equispaced points can hide at most a factor
// cos(pi*N/M) of its true peak, so dividing by that guarantees the output never exceeds
// 0 dBFS (worst-case undershoot with N=16, M=512: 0.04 dB).
peak = gains <: par(m, M / 2, abs(wave(float(m) / M))) : ba.parallelMax(M / 2) : /(cos(ma.PI * N / M));
norm = 1 / max(peak, 1e-12);   // nothing audible in the band -> silence, not NaN

// Normalize each harmonic BEFORE smoothing: a crossfade between two normalized spectra
// never exceeds 0 dBFS, whereas smoothing the gains and 1/peak separately can overshoot.
// Everything note-dependent is held while the gate is down, so the release tail keeps the
// pitch, timbre and velocity of the note that was released.
// norm is evaluated once and routed to the channels as a signal: naming it inside
// par(k, N, ...) would make the box evaluator expand the whole probe once per channel.
normalize = (si.bus(N), (_ <: si.bus(N))) : ro.interleave(N, 2) : par(k, N, *);   // N gains, norm -> N gains
hold      = ba.sAndH(gate);
smooth    = si.smooth(ba.tau2pole(smoothTau));
gainsNorm = (gains, norm) : normalize : par(k, N, hold : smooth);
env       = gate : si.lag_ud(attackTau, releaseTau);

process = gainsNorm : wave(os.lf_sawpos(f0 : hold)) * (velocity : hold : smooth) * env;
