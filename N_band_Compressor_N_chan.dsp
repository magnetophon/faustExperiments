declare name "N_band_Compressor_N_chan";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

import("stdfaust.lib");

process =
  Eight_band_Compressor_N_chan(2)
;

Eight_band_Compressor_N_chan(N) =
  crossover
  : compressors
  :mixer
   with {
  crossover =
    par(i, N, fi.crossover8LR4(1,2,3,4,5,6,7))
    :ro.interleave(8,N);
  compressors =
    par(i, 8, comp);
  comp = co.FFcompressor_N_chan(1,2,3,4,5,6,7,meter,N);
  mixer = si.bus(N*8):>si.bus(N);
  meter = _<:(_, (ba.linear2db(hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0)))):attach;
  maxGR = 30;
};



// fi.crossover2LR4(1)
// , fi.crossover3LR4(1,2)
  // , fi.crossover4LR4(1,2,3)
  // , fi.crossover8LR4(1,2,3,4,5,6,7)
  // N_band_Compressor_N_chan(10,2)
