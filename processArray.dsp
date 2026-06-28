import("stdfaust.lib");

//----------------------------`(ba.)processArray`-------------------------------
// Apply `N` parallel copies of a multi-input `processor`, feeding each copy a
// different parameter set taken stepwise from interpolated ranges. For each input
// of `processor`, a parameter array of `N` values is built between a low and a high
// bound; copy `j` receives element `j` of every array. The interpolation of each
// array is linear or logarithmic, selected per array.
//
// If fewer parameter bounds are supplied than `processor` has inputs, the remaining
// inputs are not generated but taken from `processArray`'s own signal inputs: one
// `N`-wide bus per missing parameter (a "no array" input). With `nIn = inputs(processor)`
// and `nArray = outputs(loBounds)` supplied arrays, `processArray` then has
// `(nIn-nArray)*N` inputs, ordered bus by bus, each bus `N` samples wide; copy `j`
// receives sample `j` of each bus on the inputs that have no array. When
// `nArray = nIn`, `processArray` has no signal input.
//
// #### Usage
//
// ```
// si.bus((inputs(processor)-outputs(loBounds))*N) : processArray(N, processor, isLinear, loBounds, hiBounds) : si.bus(N*outputs(processor))
// ```
//
// Where:
//
// * `N`: number of parallel copies (int, known at compile time)
// * `processor`: the function to replicate
// * `isLinear`: interpolation flag, `0` for logarithmic and `1` for linear; either a
//   single value applied to every array, or a list of one value per supplied array
// * `loBounds`: list of low bounds, one per array (at most `inputs(processor)` of them)
// * `hiBounds`: list of high bounds, matching `loBounds`
//
// #### Note:
//
// Logarithmic arrays require nonzero, same-sign bounds. The number of supplied arrays
// is measured as `outputs(loBounds)`, so `loBounds` and `hiBounds` must have the same
// length.
//
// #### Test
// ```
// ba = library("basics.lib");
// // 3-input processor, 2 arrays supplied -> 1 N-wide bus input:
// processArray_proc(a, b, c) = a + b + c;
// processArray_test = si.bus(4) : ba.processArray(4, processArray_proc, 1, (0.1, 1.0), (1.0, 10.0));
// ```
//----------------------------
processArray(N, processor, isLinear, loBounds, hiBounds) = build(nNoArray)
    with {
        nIn = inputs(processor);
        nArray = outputs(loBounds);
        nNoArray = nIn-nArray;
        flags = broadcast(outputs(isLinear))
            with {
                broadcast = case{//
                (1)=>par(i, nArray, isLinear);
                (n)=>isLinear;
                };
            };
        genArrays = (loBounds, hiBounds):ro.interleave(nArray, 2):par(i, nArray, oneArray(ba.take(i+1, flags)));
        build = case{(0)=>genArrays:ro.interleave(N, nIn):par(i, N, processor);
        (b)=>(genArrays, si.bus(nNoArray*N)):ro.interleave(N, nIn):par(i, N, processor);
        };
        oneArray = case{(0)=>logArray;
        (1)=>linArray;
        };
        linArray(lo, hi) = par(i, N, ((hi-lo)*frac(i))+lo);
        logArray(lo, hi) = par(i, N, lo*pow(hi/lo, frac(i)));
        frac(i) = i/(N-1);
    };

processArray_proc(a, b) = a*b;
processArrayBasic = si.bus(4):processArray(4, processArray_proc, 1, (0), (1));

// --- Simplest: one array, one signal input ---
// A 1-input gain whose gain is the array parameter; the audio input `x` has no
// array, so it arrives as one N-wide bus. With N=3 the three copies get gains
// 0.0, 0.5, 1.0, each applied to its own input-bus channel.
processArray_gain(g, x) = x*g;
processArray_simple = si.bus(3):processArray(3, processArray_gain, 1, (0.0), (1.0));
// --- Advanced: 5-input/2-output toy proc, 3 arrays, per-array lin/log, 2 no-array buses ---
// `amt` ramps linearly 0->1, `freqRatio` ramps logarithmically 100->8000
// (nonzero same-sign bounds, as logarithmic arrays require), `offset` ramps
// linearly -1->1. The remaining two inputs (l, r) have no array, so they arrive
// as two N-wide buses: nNoArray = 5-3 = 2 buses x N=5 = 10 signal inputs, ordered
// bus by bus. The proc returns a stereo pair, so outputs = N x 2 = 10.
// isLinear is given per array as (1,0,1).
processArray_toy(amt, freqRatio, offset, l, r) = (l*amt+offset)*freqRatio, (r*amt-offset)*freqRatio;
processArray_advanced = si.bus(10):processArray(5,
    processArray_toy,
    (1, 0, 1),
    (0.0, 100.0, -1.0),
    (1.0, 8000.0, 1.0));
process = processArray_simple;
//
//// processArrayBasic;
// processArray_advanced;
