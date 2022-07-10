declare name "Eight_band_Compander_N_chan";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

// import("stdfaust.lib");
import("/home/bart/source/faustlibraries/stdfaust.lib");

process =
  // DCblock(2)
  // : MSencode(MSon)
  // :
  Eight_band_Compander_N_chan(2)
  // : MSdecode(MSon)
;

DCblock(N) = par(i, N, ba.bypass1(fb==0,fi.dcblockerat(fb)));

MSon = checkbox("MS on");
fb = hslider("dc block", 20, 0, 50, 0.1);


Eight_band_Compander_N_chan(N) =
  // expanders
  // scPlusInputsMono
  inputGain
  : crossover
  : expanders
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
  expanders =
    EX(
      (strength_array, thresh_array, range_array, att_array, hold_array,  rel_array, knee_array , link_array,
       (ro.interleave(N,Nr_bands) :scPlusInputs ))
      :
      ro.interleave(Nr_bands,9+N)
      : par(i, Nr_bands, expander(meter(i+1),N,prePost,_,1))
    )
  ;

  scPlusInputsMono =
    si.bus(Nr_bands)
    <: ( (scMix_array
          // disgard the lowest two and the highest band, mix the rest, take the abs, div by Nr_bands, split them out:
         , ((!,!,!,!,_,_,_,!) :> abs / hslider("div", 3, 1, 5, 0.1) <: si.bus(Nr_bands))
           // , (si.bus(Nr_bands) :> abs  / Nr_bands <: si.bus(Nr_bands))
         , par(i, Nr_bands, abs)

         )
         :ro.interleave(Nr_bands,3)
         : par(i, Nr_bands, it.interpolate_linear)
       ),si.bus(Nr_bands)
  ;
  scPlusInputs = par(i, N, scPlusInputsMono)
                 :(ro.crossNM(Nr_bands*2,Nr_bands),si.bus(Nr_bands))
                 :((ro.interleave(Nr_bands,2):par(i, Nr_bands, max) ),si.bus(2*Nr_bands))
  ;

  compressors =
    CO(
      (strength_array , thresh_array , att_array , rel_array , knee_array , link_array, ro.interleave(N,Nr_bands))
      : ro.interleave(Nr_bands,6+N)
      : par(i, Nr_bands, compressor(meter(i+1),N,prePost)));
  mixer = si.bus(N*Nr_bands):>si.bus(N);
  outputGain = par(i, N, _*outGain);

  expander(meter,N,prePost,SCfunction,SCswitch,strength,thresh,range,att,hold,rel,knee,link,SCsignal) =
    co.expanderSC_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,meter,maxHold,N,SCfunction,SCswitch,SCsignal)
  with {
    maxHold = maxSR*1;
    maxSR = 192000;
  };
  compressor(meter,N,prePost,strength,thresh,att,rel,knee,link) =
    co.FFcompressor_N_chan(strength,thresh,att,rel,knee,prePost,link,ba.linear2db:meter:ba.db2linear,N);
  // co.FFcompressor_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N);
  meter(i) =
    _<:(_, (max(-40):min(0):MG(vbargraph("[%i][unit:dB]%i[tooltip: gain reduction in dB]", -40, 0)))):attach;
  // _<:(_, (ba.linear2db:max(-40):min(0):MG(vbargraph("[%i][unit:dB]%i[tooltip: gain reduction in dB]", -40, 0)))):attach;
  inGain = CG(hslider("[1]input gain", 0, -30, 30, 0.1)):ba.db2linear;
  crossoverFreqs = BT(hslider("[1]freq", 60, 20, 20000, 1),hslider("[1]freq", 8000, 20, 20000, 1)):LogArray(Nr_crossoverFreqs);
  strength_array = BTli(hslider("[2]strength", 1, 0, 8, 0.1),hslider("[2]strength", 1, 0, 8, 0.1));
  thresh_array = BTli(hslider("[3]thresh", -24, -60, 0, 0.1),hslider("[3]thresh", -24, -60, 0, 0.1));
  range_array = BTli(hslider("[4]range", -24, -60, 0, 0.1),hslider("[4]range", -24, -60, 0, 0.1));
  att_array = BTlo(hslider("[5]att", 13, 0, 100, 0.1)*0.001,hslider("[5]att", 0.1, 0, 100, 0.1)*0.001);
  hold_array = BTlo(hslider("[6]hold", 130, 1, 1000, 1)*0.001,hslider("[6]hold", 26, 1, 1000, 1)*0.001);
  rel_array = BTlo(hslider("[7]rel", 130, 1, 1000, 1)*0.001,hslider("[7]rel", 26, 1, 1000, 1)*0.001);
  knee_array = BTli(hslider("[8]knee", 0, 0, 30, 0.1),hslider("[8]knee", 30, 0, 30, 0.1));
  link_array = par(i, Nr_bands, 0);
  // link_array = BTli(hslider("[9]link", 1, 0, 1, 0.1),hslider("[9]link", 0.2, 0, 1, 0.1));
  scMix_array = BTli(hslider("[9]SC mix", 1, 0, 1, 0.1)*-1+1,hslider("[9]SC mix", 0.2, 0, 1, 0.1)*-1+1);
  outGain = CG(hslider("[3]output gain", 0, -30, 30, 0.1)):ba.db2linear;
  // make a linear array of values, from bottom to top
  LinArray(N,bottom,top) = par(i,N,   ((top-bottom)*(i/(N-1)))+bottom);
  // make a log array of values, from bottom to top
  LogArray(N,bottom,top) = par(i,N,   pow((pow((t/b),1/(N-1))),i)*b)
  with {
    b = bottom:max(ma.EPSILON);
    t = top:max(ma.EPSILON);
  };

  EX(x) = tgroup("MBcompander", vgroup("[1]expander", x));
  CO(x) = tgroup("MBcompander", vgroup("[2]compressor", x));
  CG(x) = vgroup("[2]controlls", x);
  MG(x) = hgroup("[2]gain reduction", x);
  // make a bottom and a top version of a parameter
  BT(b,t) = CG(hgroup("[3]", vgroup("[1]bottom", b),vgroup("[2]top", t)));
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
