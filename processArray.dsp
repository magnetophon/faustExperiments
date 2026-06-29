import("stdfaust.lib");

//----------------------------`(ba.)processArray`-------------------------------
// Run `N` parallel copies of `processor`, interpolating its parameters across the copies.
// For each input of `processor` you give a low and a high bound; `processArray`
// spreads that range across the `N` copies so the first copy gets the low bound,
// the last copy gets the high bound, and the copies in between get values stepping
// from low to high. Each per-input range is shaped by a supplied interpolator, so
// the steps need not be evenly spaced. With several parameters interpolated at once,
// each copy takes one value from every range: copy `j` gets the `j`-th value of each.
//
// If fewer parameter bounds are supplied than `processor` has inputs, the remaining
// inputs are not generated but taken from `processArray`'s own signal inputs: one
// `N`-wide bus per missing parameter (a "no array" input). With `nIn = inputs(processor)`
// and `nArray = outputs(loBounds)` supplied arrays, `processArray` then has
// `(nIn-nArray)*N` inputs, ordered bus by bus, each bus `N` samples wide; copy `j`
// receives sample `j` of each bus on the inputs that have no array. When
// `nArray = nIn`, `processArray` has no signal input.
//
// #### Interpolators
//
// An interpolator is a function `interp(frac, lo, hi)` returning a single value,
// where `frac` runs `0..1` stepwise across the `N` copies (`frac(j) = j/(N-1)`).
// `processArray` handles the `par(i, N, ...)` replication and hands each copy its
// own `frac`, so an interpolator only ever deals with a 0..1 fraction and works
// unchanged for any `N`. You can pass one interpolator, which shapes every parameter
// the same way, or a list with one interpolator per parameter, to shape each
// parameter differently.
//
// Interpolators take `frac` *first*, matching `interpolators.lib`'s `(dv, v0, v1)`
// order, so `it.interpolate_linear` / `it.interpolate_cosine` can be passed straight
// through with no wrapper. Parametric interpolators put their own parameter ahead of
// `frac` so it can be curried off: `powInterp(2)` fixes the exponent and leaves a
// ready-to-use `(frac, lo, hi)` interpolator.
//
// Built-in interpolators (all live in basics.lib as `ba.*`):
//
// * `ba.logInterp(frac, lo, hi)` — logarithmic; requires nonzero, same-sign bounds.
//   Not available in `interpolators.lib` (no ratio-law interpolator there).
// * `ba.powInterp(p, frac, lo, hi)` — power/skew; partially apply `p`
//   (`ba.powInterp(2)` biases toward `lo`, `ba.powInterp(0.5)` toward `hi`).
//   Not available in `interpolators.lib` (no curvature parameter there).
//
// For linear and cosine interpolation, pass `it.interpolate_linear` /
// `it.interpolate_cosine` directly — they already have the `(frac, lo, hi)` shape.
//
// A custom interpolator is any function of the same `(frac, lo, hi)` shape, e.g.
// `myInterp(frac, lo, hi) = lo + (hi-lo)*frac*frac;`.
//
// #### Usage
//
// ```
// si.bus((inputs(processor)-outputs(loBounds))*N) : processArray(N, processor, interp, loBounds, hiBounds) : si.bus(N*outputs(processor))
// ```
//
// Where:
//
// * `N`: number of parallel copies (int, known at compile time, `N >= 2`)
// * `processor`: the function to replicate
// * `interp`: interpolator function(s) of shape `interp(frac, lo, hi)`; either a
//   single function applied to every array, or a list of one function per supplied array
// * `loBounds`: list of low bounds, one per array (at most `inputs(processor)` of them)
// * `hiBounds`: list of high bounds, matching `loBounds`
//
// #### Note:
//
// The number of supplied arrays is measured as `outputs(loBounds)`, so `loBounds`,
// `hiBounds`, and (when passed as a list) `interp` must all have that same length.
//
// #### Test
// ```
// ba = library("basics.lib");
// it = library("interpolators.lib");
// // 3-input processor, 2 arrays supplied -> 1 N-wide bus input:
// processArray_proc(a, b, c) = a + b + c;
// processArray_test = si.bus(4) : ba.processArray(4, processArray_proc, it.interpolate_linear, (0.1, 1.0), (1.0, 10.0));
// ```
//----------------------------

// --- Built-in interpolators (these belong in basics.lib as ba.*) ---
// lin and cos already exist in interpolators.lib as it.interpolate_linear /
// it.interpolate_cosine with this exact (frac, lo, hi) shape, so no wrappers are
// needed — pass those directly. log and pow have no equivalent there, so they are
// defined here.
logInterp(frac, lo, hi) = lo*pow(hi/lo, frac);
powInterp(p, frac, lo, hi) = lo+(hi-lo)*pow(frac, p);

processArray(N, processor, interp, loBounds, hiBounds) = build(nNoArray)
    with {
        nIn = inputs(processor);
        nArray = outputs(loBounds);
        nNoArray = nIn-nArray;
        // `interp` arrives in one of two shapes: a single interpolator (to use for
        // every array) or a list of one interpolator per array. Normalize both to the
        // list form so the code below can index into it uniformly.
        //
        // The two shapes are told apart by output count: build nArray parallel copies
        // of `interp` and count their outputs. When that count is 1, treat `interp` as
        // a single function and replicate it into an nArray-long list; otherwise
        // `interp` is already a list and passes through untouched. (This is a port of
        // the original boolean isLinear flag-broadcasting trick.)
        interps = broadcast(interpCount)
            with {
                interpCount = outputs(par(i, nArray, interp));
                broadcast = case{(1)=>par(i, nArray, interp);
                (n)=>interp;
                };
            };
        // loBounds and hiBounds arrive as one flat list: every low bound first, then
        // every high bound. interleave(nArray, 2) zips them back into nArray (lo, hi)
        // pairs — one per array. oneArray then expands each pair into its own N-wide
        // bus of interpolated values.
        genArrays = (loBounds, hiBounds):ro.interleave(nArray, 2):par(i, nArray, oneArray(ba.take(i+1, interps)));
        // genArrays lays its output out parameter-by-parameter: all N values of
        // parameter 0, then all N values of parameter 1, and so on. The processor
        // instances need the opposite grouping — each instance wants one value from
        // every parameter. interleave(N, nIn) performs that transpose, turning the nIn
        // parameter-blocks into N per-copy blocks, which then feed the N processor
        // instances. When no-array inputs are present, their nNoArray*N buses (laid out
        // the same parameter-by-parameter way) are appended first, so the same
        // transpose lines everything up.
        build = case{(0)=>genArrays:ro.interleave(N, nIn):par(i, N, processor);
        (b)=>(genArrays, si.bus(nNoArray*N)):ro.interleave(N, nIn):par(i, N, processor);
        };
        // Replicate one array: apply the chosen interpolator at each frac step.
        oneArray(thisInterp, lo, hi) = par(i, N, thisInterp(frac(i), lo, hi));
        // frac runs 0..1 across the N copies. NOTE: divides by N-1, so N must be >= 2.
        frac(i) = i/(N-1);
    };

// ====================== TESTS (adapted from the originals) ======================

processArray_proc(a, b) = a*b;
processArrayBasic = si.bus(4):processArray(4, processArray_proc, it.interpolate_linear, (0), (1));

// --- Simplest: one array, one signal input ---
// A 1-input gain whose gain is the array parameter; the audio input `x` has no
// array, so it arrives as one N-wide bus. With N=3 the three copies get gains
// 0.0, 0.5, 1.0, each applied to its own input-bus channel.
processArray_gain(g, x) = x*g;
processArray_simple = si.bus(3):processArray(3, processArray_gain, it.interpolate_linear, (0.0), (1.0));

// --- Advanced: 5-input/2-output toy proc, 3 arrays, per-array interp, 2 no-array buses ---
// `amt` ramps linearly 0->1, `freqRatio` ramps logarithmically 100->8000
// (nonzero same-sign bounds, as logInterp requires), `offset` ramps with a
// power/skew curve -1->1. The remaining two inputs (l, r) have no array, so they
// arrive as two N-wide buses: nNoArray = 5-3 = 2 buses x N=5 = 10 signal inputs.
// The proc returns a stereo pair, so outputs = N x 2 = 10.
// interp is given per array as a list: (it.interpolate_linear, logInterp, powInterp(2)).
processArray_toy(amt, freqRatio, offset, l, r) = (l*amt+offset)*freqRatio, (r*amt-offset)*freqRatio;
processArray_advanced = si.bus(10):processArray(5,
    processArray_toy,
    (it.interpolate_linear, logInterp, powInterp(2)),
    (0.0, 100.0, -1.0),
    (1.0, 8000.0, 1.0));

process = processArray_advanced;
