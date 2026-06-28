import("stdfaust.lib");

//----------------------------`(ba.)processArray`-------------------------------
// Apply `N` parallel copies of a multi-input `processor`, feeding each copy a
// different parameter set taken stepwise from interpolated ranges. For each input
// of `processor`, a parameter array of `N` values is built between a low and a high
// bound; copy `j` receives element `j` of every array. The interpolation of each
// array is performed by a supplied interpolator function, selected per array.
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
// An interpolator is a function `interp(lo, hi, frac)` returning a single value,
// where `frac` runs `0..1` stepwise across the `N` copies (`frac(j) = j/(N-1)`).
// `processArray` owns the `par(i, N, ...)` replication and feeds each copy its own
// `frac`; the interpolator never sees `N`. The `interp` argument is either a single
// function applied to every array, or a list of one function per supplied array.
//
// Argument order puts `frac` *last* (the reverse of `interpolators.lib`'s
// `(dv, v0, v1)`) so parametric interpolators can be curried: `powInterp(2)` fixes
// the exponent and leaves a ready-to-use `(lo, hi, frac)` interpolator, which
// `frac`-first would not allow. The cost is that `it.interpolate_linear` /
// `it.interpolate_cosine` can't be passed directly, so `linInterp` / `cosInterp`
// below are thin argument-reordering wrappers around them.
//
// Built-in interpolators (all live in basics.lib as `ba.*`):
//
// * `ba.linInterp(lo, hi, frac)` — linear; wraps `it.interpolate_linear`.
// * `ba.cosInterp(lo, hi, frac)` — raised-cosine S-curve; wraps `it.interpolate_cosine`.
// * `ba.logInterp(lo, hi, frac)` — logarithmic; requires nonzero, same-sign bounds.
//   Not available in `interpolators.lib` (no ratio-law interpolator there).
// * `ba.powInterp(p, lo, hi, frac)` — power/skew; partially apply `p`
//   (`ba.powInterp(2)` biases toward `lo`, `ba.powInterp(0.5)` toward `hi`).
//   Not available in `interpolators.lib` (no curvature parameter there).
//
// A custom interpolator is any function of the same `(lo, hi, frac)` shape, e.g.
// `myInterp(lo, hi, frac) = lo + (hi-lo)*frac*frac;`.
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
// * `interp`: interpolator function(s) of shape `interp(lo, hi, frac)`; either a
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
// // 3-input processor, 2 arrays supplied -> 1 N-wide bus input:
// processArray_proc(a, b, c) = a + b + c;
// processArray_test = si.bus(4) : ba.processArray(4, processArray_proc, ba.linInterp, (0.1, 1.0), (1.0, 10.0));
// ```
//----------------------------

// --- Built-in interpolators (these belong in basics.lib as ba.*) ---
// lin and cos reuse interpolators.lib; we only reorder its args to put frac last,
// so parametric interpolators (powInterp) stay curry-able. See the header note.
linInterp(lo, hi, frac) = it.interpolate_linear(frac, lo, hi);
cosInterp(lo, hi, frac) = it.interpolate_cosine(frac, lo, hi);
// log and pow have no equivalent in interpolators.lib, so they are defined here.
logInterp(lo, hi, frac) = lo*pow(hi/lo, frac);
powInterp(p, lo, hi, frac) = lo+(hi-lo)*pow(frac, p);

processArray(N, processor, interp, loBounds, hiBounds) = build(nNoArray)
    with {
        nIn = inputs(processor);
        nArray = outputs(loBounds);
        nNoArray = nIn-nArray;
        // Broadcast a single interpolator to one-per-array, or pass a list through.
        // outputs(par(i,nArray,interp)) is nArray when interp is a single function,
        // and a larger count when interp is already an nArray-long list of functions;
        // case-match on that to tell the two callers apart (mirrors the original
        // isLinear flag-broadcasting machinery).
        interps = broadcast(interpCount)
            with {
                interpCount = outputs(par(i, nArray, interp));
                broadcast = case{(1)=>par(i, nArray, interp);
                (n)=>interp;
                };
            };
        // (loBounds, hiBounds) arrive concatenated as all-los then all-his;
        // interleave(nArray, 2) regroups them into nArray (lo,hi) pairs, one per
        // array, then each pair is expanded into its N-wide bus by oneArray.
        genArrays = (loBounds, hiBounds):ro.interleave(nArray, 2):par(i, nArray, oneArray(ba.take(i+1, interps)));
        // genArrays emits nArray N-wide buses, parameter-major: all N copies of
        // parameter 0, then all N copies of parameter 1, etc. interleave(N, nIn)
        // transposes that into N groups of nIn (each group = one copy's full
        // parameter set) and fans them out to the N processor instances. With
        // no-array inputs present, append their nNoArray*N buses (also
        // parameter-major) before the same transpose.
        build = case{(0)=>genArrays:ro.interleave(N, nIn):par(i, N, processor);
        (b)=>(genArrays, si.bus(nNoArray*N)):ro.interleave(N, nIn):par(i, N, processor);
        };
        // Replicate one array: apply the chosen interpolator at each frac step.
        oneArray(thisInterp, lo, hi) = par(i, N, thisInterp(lo, hi, frac(i)));
        // frac runs 0..1 across the N copies. NOTE: divides by N-1, so N must be >= 2.
        frac(i) = i/(N-1);
    };

// ====================== TESTS (adapted from the originals) ======================

processArray_proc(a, b) = a*b;
processArrayBasic = si.bus(4):processArray(4, processArray_proc, linInterp, (0), (1));

// --- Simplest: one array, one signal input ---
// A 1-input gain whose gain is the array parameter; the audio input `x` has no
// array, so it arrives as one N-wide bus. With N=3 the three copies get gains
// 0.0, 0.5, 1.0, each applied to its own input-bus channel.
processArray_gain(g, x) = x*g;
processArray_simple = si.bus(3):processArray(3, processArray_gain, linInterp, (0.0), (1.0));

// --- Advanced: 5-input/2-output toy proc, 3 arrays, per-array interp, 2 no-array buses ---
// `amt` ramps linearly 0->1, `freqRatio` ramps logarithmically 100->8000
// (nonzero same-sign bounds, as logInterp requires), `offset` ramps with a
// power/skew curve -1->1. The remaining two inputs (l, r) have no array, so they
// arrive as two N-wide buses: nNoArray = 5-3 = 2 buses x N=5 = 10 signal inputs.
// The proc returns a stereo pair, so outputs = N x 2 = 10.
// interp is given per array as a list: (linInterp, logInterp, powInterp(2)).
processArray_toy(amt, freqRatio, offset, l, r) = (l*amt+offset)*freqRatio, (r*amt-offset)*freqRatio;
processArray_advanced = si.bus(10):processArray(5,
    processArray_toy,
    (linInterp, logInterp, powInterp(2)),
    (0.0, 100.0, -1.0),
    (1.0, 8000.0, 1.0));

process = processArray_advanced;
