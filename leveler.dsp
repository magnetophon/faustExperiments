declare name "MutiStageDynamics";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

import("stdfaust.lib");

ebu = library("lib/ebur128.dsp");
ex = library("expanders.lib");
///////////////////////////////////////////////////////////////////////////////
//                             working principle                             //
///////////////////////////////////////////////////////////////////////////////


/*
A multi stage dynamics processor.
Each stage determines the speed of the next one.
Only the last stage
*/


process =
  leveler_sc(target) ~ (_,_);


target = hslider("target", init_leveler_target, -23, -6, 1);

init_leveler_target = -23;
init_leveler_maxboost = 40;
init_leveler_maxcut = 40;

bp = checkbox("v:soundsgood/t:expert/h:[3]leveler/[1]leveler bypass[symbol:leveler_bypass]") : si.smoo;
limit_pos = vslider("v:soundsgood/t:expert/h:[3]leveler/[7][symbol:leveler_max_plus][unit:db]leveler max +", init_leveler_maxboost, 0, 60, 1);
limit_neg = vslider("v:soundsgood/t:expert/h:[3]leveler/[8][symbol:leveler_max_minus][unit:db]leveler max -", init_leveler_maxcut, 0, 60, 1) : ma.neg;
// GT4 2014-2015 Modification of a sine wave using a 3 stage NTSF chain. Version 1. Dino Dini. 29/04/2015
// https://www.desmos.com/calculator/nbzgwyd7fz

leveler_sc(target,fl,fr,l,r) =
  (calc(
      lk2_momentary(fl,fr)
     ,lk2_short(gate(target-lk2_momentary(fl,fr)),fl,fr)
     , lk2_long(gate(target-lk2_momentary(fl,fr)),fl,fr))
   *(1-bp)+bp)
  <: (_*l,_*r)
with {

  gate(dif_momentary) = dif_momentary<10:hbargraph("gate", 0, 1);
  calc(momentary,short,long) = FB(momentary,short,long)~_: ba.db2linear
  with {

    FB(momentary,short,long,prev_gain) =
      dif_momentary
      +(prev_gain )
      : limit(limit_neg,limit_pos)
        <: si.onePoleSwitching(release,attack)
      : leveler_meter_gain
    ;
    dif_momentary = target - momentary:hbargraph("dif momentary[unit:dB]", -24, 24);
    dif_short = target - short:hbargraph("dif short[unit:dB]", -24, 24);
    dif_long = target - long:hbargraph("dif long[unit:dB]", -24, 24);

    short_over = dif_short*-1:max(ma.EPSILON);
    short_under = dif_short:max(ma.EPSILON);

    long_over = dif_long*-1:max(ma.EPSILON);
    long_under = dif_long:max(ma.EPSILON);

    mult(level,range) =  level / range:min(1);

    dead_range = hslider("dead range", 6, 0.1, 24, 0.1);
    long_dead_range = hslider("long_dead range", 3, 0.1, 24, 0.1);

    leveler_meter_gain = hbargraph("v:soundsgood/h:easy/[4][unit:dB][symbol:leveler_gain]leveler gain",-40,40);


    attack =
      hslider("attack", 1, 0.1, 10, 1)
      / ( (mult(short_over,dead_range) * mult(long_over, long_dead_range))
          // : si.onePoleSwitching(long_attack,long_release)long_attack
          *gate(target-lk2_momentary(fl,fr))
          :hbargraph("att speed", 0, 1))
    ;
    release =
      hslider("release", 5, 0.1, 30, 1)
      / ( (mult(short_under,dead_range) * mult(long_under,long_dead_range))
          // : si.onePoleSwitching(long_release,long_attack)
          *gate(target-lk2_momentary(fl,fr))
          :hbargraph("rel speed", 0, 1));

    long_attack =
      hslider("long attack", 1, 0.1, 10, 1)
      / (mult(long_over)
         :hbargraph("long att speed", 0, 1))
    ;
    long_release =
      hslider("long release", 5, 0.1, 30, 1)
      / (mult(long_under)
         :hbargraph("long rel speed", 0, 1));
  };
};

limit(minimum, maximum) = max(minimum):min(maximum);

lk2_var(Tg)= par(i,2,kfilter : zi) :> 4.342944819 * log(max(1e-12)) : -(0.691) with {
  // maximum assumed sample rate is 192k
  maxSR = 192000;
  sump(n) = ba.slidingSump(n, Tg*maxSR)/max(n,1);
  envelope(period, x) = x * x :  sump(rint(period * ma.SR));
  //Tg = 3; // 3 second window for 'short-term' measurement
  zi = envelope(Tg); // mean square: average power = energy/Tg = integral of squared signal / Tg

  kfilter = ebu.prefilter;
};

lk2_var_gated(Tg,gate)= par(i,2,kfilter : zi) :> 4.342944819 * log(max(1e-12)) : -(0.691) with {
  // maximum assumed sample rate is 192k
  maxSR = 192000;
  sump(n) = ba.slidingSump(n, Tg*maxSR)/max(n,1);
  envelope(period,gate, x) =
    (select2(gate,_,x*x)
     : sump(rint(period * ma.SR)))~_;
  //Tg = 3; // 3 second window for 'short-term' measurement
  zi = envelope(Tg,gate); // mean square: average power = energy/Tg = integral of squared signal / Tg

  kfilter = ebu.prefilter;
};

lufs_meter_in(l,r) = l,r <: l, attach(r, (lk2_short : vbargraph("v:soundsgood/h:easy/[2][unit:dB][symbol:lufs_in]in lufs-s",-70,0))) : _,_;
lufs_meter_out(l,r) = l,r <: l, attach(r, (lk2_short : vbargraph("v:soundsgood/h:easy/[7][unit:dB][symbol:lufs_out]out lufs-s",-70,0))) : _,_;

lk2_momentary = lk2_var(0.4);
lk2_short(gate) = lk2_var_gated(3,gate);
lk2_long(gate) = lk2_var_gated(22.5,gate);
