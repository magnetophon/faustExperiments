declare name "N_band_Compressor_N_chan";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

import("stdfaust.lib");

process =
  Eight_band_Compressor_N_chan(2) ;

Eight_band_Compressor_N_chan(N) =
  inputGain
  : crossover
  : compressors
  :mixer
with {
  inputGain = par(i, N, _*inGain);
  crossover =
    (
      (crossoverFreqs<:par(i, Nr_crossoverFreqs, _<:si.bus(N)))
     ,si.bus(N)
    )
    <: ro.interleave(N,Nr_bands)
    :  par(i, N, fi.crossover8LR4)
    :  ro.interleave(Nr_bands,N);
  compressors =
    (strength_array , thresh_array , att_array , rel_array , knee_array , link_array, ro.interleave(N,Nr_bands))
    : ro.interleave(Nr_bands,6+N)
    : par(i, Nr_bands, compressor(meter(i+1),N,prePost))
  ;
  compressor(meter,N,prePost,strength,thresh,att,rel,knee,link) =
    co.FFcompressor_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N);
  mixer = si.bus(N*Nr_bands):>si.bus(N);
  meter(i) = _<:(_, (ba.linear2db:MG(vbargraph("[%i][unit:dB]%i[tooltip: gain reduction in dB]", -40, 0)))):attach;
  maxGR = -30;
  inGain = CG(hslider("[1]inputGain", 0, -30, 30, 0.1)):ba.db2linear;
  crossoverFreqs = BT(hslider("[1]freq", 80, 20, 20000, 1)):LogArray(Nr_crossoverFreqs);
  strength_array = BTli(hslider("[2]strength", 1, 0, 8, 0.1));
  thresh_array = BTli(hslider("[3]thresh", 0, -60, 0, 0.1));
  att_array = (BTlo(hslider("[4]att", 1, 0, 100, 0.1)*0.001));
  rel_array = BTlo(hslider("[5]rel", 42, 1, 1000, 1)*0.001);
  knee_array = BTli(hslider("[6]knee", 3, 0, 30, 0.1));
  link_array = BTlo(hslider("[7]link", 1, 0, 1, 0.1));
  // make a linear array of values, from bottom to top
  LinArray(N,bottom,top) = par(i,N,   ((top-bottom)*(i/(N-1)))+bottom);
  // make a log array of values, from bottom to top
  LogArray(N,bottom,top) = par(i,N,   pow((pow((top/b),1/(N-1))),i)*b)
  with {
    b = bottom:max(ma.EPSILON);
  };

  CG(x) = vgroup("[1]controlls", x);
  MG(x) = hgroup("[2]gain reduction", x);
  // make a bottom and a top version of a parameter
  BT(x) = CG(hgroup("[2]", vgroup("[1]bottom", x),vgroup("[2]top", x)));
  BTlo(x) = BT(x):LogArray(Nr_bands);
  BTli(x) = BT(x):LinArray(Nr_bands);
  Nr_bands = 8;
  Nr_crossoverFreqs = Nr_bands-1;
  prePost = 1;
};
