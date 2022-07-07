declare name "N_band_Compressor_N_chan";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

import("stdfaust.lib");
// import("/home/bart/source/faustlibraries/stdfaust.lib");

process =
  DCblock(2)
  : MSencode(MSon)
  : Eight_band_Compressor_N_chan(2)
  : MSdecode(MSon)
;

DCblock(N) = par(i, N, fi.dcblockerat(fb));

MSon = checkbox("MS on");
fb = hslider("dc block", 20, 0, 50, 0.1);


Eight_band_Compressor_N_chan(N) =
  inputGain
  : crossover
  : compressors
  : mixer
  : outputGain
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
    : par(i, Nr_bands, compressor(meter(i+1),N,prePost)) ;
  mixer = si.bus(N*Nr_bands):>si.bus(N);
  outputGain = par(i, N, _*outGain);

  compressor(meter,N,prePost,strength,thresh,att,rel,knee,link) =
    co.FFcompressor_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N);
  meter(i) =
    // _<:(_, (max(-40):min(0):MG(vbargraph("[%i][unit:dB]%i[tooltip: gain reduction in dB]", -40, 0)))):attach;
    _<:(_, (ba.linear2db:max(-40):min(0):MG(vbargraph("[%i][unit:dB]%i[tooltip: gain reduction in dB]", -40, 0)))):attach;
  inGain = CG(hslider("[1]input gain", 0, -30, 30, 0.1)):ba.db2linear;
  crossoverFreqs = BT(hslider("[1]freq", 60, 20, 20000, 1),hslider("[1]freq", 8000, 20, 20000, 1)):LogArray(Nr_crossoverFreqs);
  strength_array = BTli(hslider("[2]strength", 1, 0, 8, 0.1),hslider("[2]strength", 1, 0, 8, 0.1));
  thresh_array = BTli(hslider("[3]thresh", -24, -60, 0, 0.1),hslider("[3]thresh", -24, -60, 0, 0.1));
  att_array = BTlo(hslider("[4]att", 13, 0, 100, 0.1)*0.001,hslider("[4]att", 0.1, 0, 100, 0.1)*0.001);
  rel_array = BTlo(hslider("[5]rel", 130, 1, 1000, 1)*0.001,hslider("[5]rel", 26, 1, 1000, 1)*0.001);
  knee_array = BTli(hslider("[6]knee", 0, 0, 30, 0.1),hslider("[6]knee", 30, 0, 30, 0.1));
  link_array = BTli(hslider("[7]link", 1, 0, 1, 0.1),hslider("[7]link", 0.2, 0, 1, 0.1));
  outGain = CG(hslider("[3]output gain", 0, -30, 30, 0.1)):ba.db2linear;
  // make a linear array of values, from bottom to top
  LinArray(N,bottom,top) = par(i,N,   ((top-bottom)*(i/(N-1)))+bottom);
  // make a log array of values, from bottom to top
  LogArray(N,bottom,top) = par(i,N,   pow((pow((t/b),1/(N-1))),i)*b)
  with {
    b = bottom:max(ma.EPSILON);
    t = top:max(ma.EPSILON);
  };
  CG(x) = vgroup("[1]controlls", x);
  MG(x) = hgroup("[2]gain reduction", x);
  // make a bottom and a top version of a parameter
  BT(b,t) = CG(hgroup("[2]", vgroup("[1]bottom", b),vgroup("[2]top", t)));
  BTlo(b,t) = BT(b,t):LogArray(Nr_bands);
  BTli(b,t) = BT(b,t):LinArray(Nr_bands);
  Nr_bands = 8;
  Nr_crossoverFreqs = Nr_bands-1;
  prePost = 1;
  maxGR = -30;
};

MSencode(on,l,r) =
  select2(on
         , l
         , ((l+r)/sqrt(2)))
, select2(on
         , r
         , ((l-r)/sqrt(2)));
MSdecode(on,m,s) =
  select2(on
         , m
         , ((m+s)/sqrt(2)))
, select2(on
         , s
         , ((m-s)/sqrt(2)));
