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
  // limiter
  (leveler:limiter)
with {
  leveler(x,y) =
    (levelerGain_lin(x,y)*x)
  , (levelerGain_lin(x,y)*y);
  levelerGain_lin(x,y) =
    maxLevel(x,y):max(0)*-1
    : ba.slidingMin(holdSam,2^16)
    : si.onePoleSwitching(levRel,levAtt)
    : si.onePoleSwitching(levRel,levAtt)
    : levGr(meter)
    : ba.db2linear;
  limiterGain(x,y) =
    (maxLevel(x,y)):max(0)
    : si.onePoleSwitching(0,limRel)
    : si.onePoleSwitching(0,limRel)
      *-1 ;
  limiter(x,y) =
    (x*limiterGain_lin(x,y)),(y*limiterGain_lin(x,y));
  limiterGain_lin(x,y) =
    limiterGain(x,y)
    :limGr(meter)
    :ba.db2linear;
  maxLevel(x,y) = max(abs(x),abs(y)):ba.linear2db;
  holdSam = ba.sec2samp(levHold):int:max(1);
};

input_gain = hslider("[0]input gain", 0, -24, 24, 1);

meter = hbargraph("[0]gain reduction", -24, 0);

levGr(x) = vgroup("[1]leveler", x);
levAtt = levGr(hslider("[1]attack", 1, 0, 2, 0.05))*0.5;
levHold = levGr(hslider("[2]hold", 0.15, 0, 1, 0.01));
levRel = levGr(hslider("[3]release", 0.5, 0, 1, 0.05))*0.5;

limGr(x) = vgroup("[2]limiter", x);
limRel = limGr(hslider("[1]release", 0.25, 0.05, 0.5, 0.01))*0.5;
