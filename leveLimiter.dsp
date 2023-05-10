declare name "leveLimiter";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPL-3";

import("stdfaust.lib");

process =
  par(i, 2, _*ba.db2linear(input_gain))
  : leveLimiter
;

leveLimiter =
  // , limiter
 (leveler:limiter)
 // ba.bypass_fade(n, checkbox("lev on"), leveler)
 // : limiter
with {
  leveler(x,y) =
    (levelerGain_lin(x,y)*x)
  , (levelerGain_lin(x,y)*y);
  levelerGain_lin(x,y) =
    maxLevel(x,y):max(0)*-1
    : ba.slidingMin(holdSam,maxHold)
      // *levOn
    : si.onePoleSwitching(levRel*(1-levShape),levAtt*(1-levShape))
    : seq(i, smNr, si.onePoleSwitching((levRel/smNr)*levShape,(levAtt/smNr)*levShape))
    : meterGr(levGr(meter))
    : ba.db2linear;
  limiterGain(x,y) =
    (maxLevel(x,y)):max(0)
    : si.onePoleSwitching(0,limRel*(1-limShape))
    :seq(i, smNr, si.onePoleSwitching(0,(limRel/smNr)*limShape))
     *-1 ;
  limiter(x,y) =
    (x*limiterGain_lin(x,y)),(y*limiterGain_lin(x,y));
  limiterGain_lin(x,y) =
    limiterGain(x,y)
    :meterGr(limGr(meter))
    :ba.db2linear;
  maxLevel(x,y) = max(abs(x),abs(y)):ba.linear2db;
  holdSam = ba.sec2samp(levHold):int:max(1);
  n = ma.SR*0.1;
};

input_gain = hslider("[0]input gain", 0, -24, 24, 1):si.smoo;

meterGr(x) = vgroup("meters", x);
meter = hbargraph("[0]gain reduction [unit:dB]", -24, 0);

levGr(x) = vgroup("[1]leveler", x);
levOn = levGr(checkbox("[0]leveler on"));
levAtt = levGr(hslider("[1]attack", 1, 0, 2, 0.05));
levHold = levGr(hslider("[2]hold", 0.01, 0, maxHold/maxSR, 0.01));
levRel = levGr(hslider("[4]release", 0.5, 0, 1, 0.05));
levShape = levGr(hslider("[5]shape", 0.25, 0, 1, 0.01));
smNr = 16;

limGr(x) = vgroup("[2]limiter", x);
limRel = limGr(hslider("[1]release", 0.25, 0.05, 0.5, 0.01));
limShape = limGr(hslider("[2]shape", 0.25, 0, 1, 0.01));

maxSR = 192000;
maxHold = 2^16;
