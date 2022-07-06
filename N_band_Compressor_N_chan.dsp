declare name "N_band_Compressor_N_chan";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

import("stdfaust.lib");

process =
  Eight_band_Compressor_N_chan(3)
;

Eight_band_Compressor_N_chan(N) =
  crossover
  : compressors
  :mixer
with {
  crossover =
    (
      (crossoverFreqs<:par(i, 7, _<:si.bus(N)))
     ,si.bus(N)
    )
    <: ro.interleave(N,8)
    :  par(i, N, fi.crossover8LR4)
    :  ro.interleave(8,N);
  compressors =
    par(i, 8, comp);
  comp = co.FFcompressor_N_chan(1,2,3,4,5,6,8,meter,N);
  mixer = si.bus(N*8):>si.bus(N);
  meter = _<:(_, (ba.linear2db(hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0)))):attach;
  maxGR = 30;
  crossoverFreqs = BT(hslider("freq", 80, 20, 20000, 1)):LogArray(7);
  // make a log array of values, from bottom to top
  LogArray(nrElements,bottom,top) =     par(i,nrElements,   pow((pow((top/bottom),1/(nrElements-1))),i)*bottom);
  // make a bottom and a top version of a parameter
  BT(x) = hgroup("BT", vgroup("bottom", x),vgroup("top", x));
};
