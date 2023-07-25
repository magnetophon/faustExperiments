declare name "leveler";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPLv3";

import("stdfaust.lib");
// import("lufs_meter.dsp");
// LEVELER

ebu = library("lib/ebur128.dsp");
ex = library("expanders.lib");



init_leveler_target = -18;
init_leveler_maxboost = 40;
init_leveler_maxcut = 40;
init_leveler_gatethreshold = -24;
init_leveler_speed = 25;

process = leveler_sc(target)~(_,_);

target = hslider("target", -14, -23, -6, 1);




leveler_sc(target,fl,fr,l,r) =
  (calc(lk2(fl,fr),lk2_short(fl,fr))*(1-bp)+bp)
  <: (_*l,_*r)
with {

  calc(lufs,short_lufs) = FB(lufs,short_lufs)~_: ba.db2linear;
  FB(lufs,short_lufs,prev_gain) =
    (target - short_lufs)
    +(prev_gain )
    :  limit(limit_neg,limit_pos)
    : si.onePoleSwitching(release,attack)
    : leveler_meter_gain
  with {
    long_lufs = short_lufs:si.onePoleSwitching(long_length*0.22,long_length);
    long_diff = (target-long_lufs)
                // : hbargraph("[2]long diff[unit:dB]", -30, 30)
    ;
    undead_att =
      long_diff:min(0)/(0-(long_length:max(ma.EPSILON))):min(1):pow(hslider("att pow", 2, 1, 10, 0.1))*hslider("att mul", 69, 1, 100, 0.1):autoSat:hbargraph("att speed", 0, 1) ;
    undead_rel =
      long_diff:max(0)/(long_length:max(ma.EPSILON)):min(1):pow(hslider("att pow", 2, 1, 10, 0.1))*hslider("att mul", 69, 1, 100, 0.1):autoSat:hbargraph("rel speed", 0, 1) ;
    // attack = att / (((speedfactor*(1-undead_att))+undead_att):max(ma.EPSILON));
    // release = rel * (leveler_expander*ma.MAX+1) / (((speedfactor*(1-undead_rel))+undead_rel):max(ma.EPSILON));
    attack = att / (undead_att:max(ma.EPSILON));
    release = rel * (leveler_expander*ma.MAX+1) / (undead_rel:max(ma.EPSILON));
    diff = abs(target - lufs);
    speedfactor = ( autoSat(
                      (diff/(deadzone*0.5)
                      ) -1
                    ) +1
                  ) *0.5 :pow(hslider("power", 1, 0.1, 10, 0.1));
  };


  limit(lo,hi) = min(hi) : max(lo);

  speed_scale = (1.6-leveler_speed);
  att = speed_scale *
        6;
  // hslider("[98]att[unit:s]", 3, 0, 10, 0.1);
  rel = speed_scale *
        13;
  long_length =
    (1-leveler_speed):pow(3)*42+6;
  deadzone =
    long_length;
  // from: https://github.com/zamaudio/zam-plugins/blob/8cd23d781018e3ec84159958d3d2dc7038a82736/plugins/ZamAutoSat/ZamAutoSatPlugin.cpp#L71
  autoSat(x) = x:min(1):max(-1)<:2.0*_ * (1.0-abs(_)*0.5);
  leveler_expander =
    1-(ex.peak_expansion_gain_mono_db(maxHold,strength,leveler_gate_thresh,range,gate_att,hold,gate_rel,knee,prePost,abs(fl)+abs(fr))
       : ba.db2linear
       :max(0)
       :min(1)
       : meter_leveler_gate);
  maxHold = hold*192000;
  strength = 2;
  range = 0-ma.MAX;
  gate_att = 0;
  hold = 0.1;
  gate_rel = 0.1;
  knee = 30;
  prePost = 1;

  bp = checkbox("v:soundsgood/t:expert/h:[3]leveler/[1]leveler bypass[symbol:leveler_bypass]") : si.smoo;
  leveler_meter_gain = hbargraph("v:soundsgood/h:easy/[4][unit:dB][symbol:leveler_gain]leveler gain",-40,40);
  meter_leveler_gate =  vbargraph("v:soundsgood/t:expert/h:[3]leveler/[6][unit:%]leveler gate[symbol:leveler_gate]",0,1);

  leveler_speed = vslider("v:soundsgood/t:expert/h:[3]leveler/[4][unit:%][symbol:leveler_speed]leveler speed", init_leveler_speed, 0, 100, 1) * 0.01;
  leveler_gate_thresh = target + vslider("v:soundsgood/t:expert/h:[3]leveler/[5][unit:db][symbol:leveler_gate_threshold]leveler gate threshold", init_leveler_gatethreshold,-90,0,1);

  limit_pos = vslider("v:soundsgood/t:expert/h:[3]leveler/[7][symbol:leveler_max_plus][unit:db]leveler max +", init_leveler_maxboost, 0, 60, 1);
  limit_neg = vslider("v:soundsgood/t:expert/h:[3]leveler/[8][symbol:leveler_max_minus][unit:db]leveler max -", init_leveler_maxcut, 0, 60, 1) : ma.neg;
  length = 0.4;
};
// +++++++++++++++++++++++++ LUFS METER +++++++++++++++++++++++++

lk2_var(Tg)= par(i,2,kfilter : zi) :> 4.342944819 * log(max(1e-12)) : -(0.691) with {
  // maximum assumed sample rate is 192k
  maxSR = 192000;
  sump(n) = ba.slidingSump(n, Tg*maxSR)/max(n,ma.EPSILON);
  envelope(period, x) = x * x :  sump(rint(period * ma.SR));
  //Tg = 3; // 3 second window for 'short-term' measurement
  zi = envelope(Tg); // mean square: average power = energy/Tg = integral of squared signal / Tg

  kfilter = ebu.prefilter;
};

lufs_meter_in(l,r) = l,r <: l, attach(r, (lk2 : vbargraph("v:soundsgood/h:easy/[2][unit:dB][symbol:lufs_in]in lufs-s",-70,0))) : _,_;
lufs_meter_out(l,r) = l,r <: l, attach(r, (lk2 : vbargraph("v:soundsgood/h:easy/[7][unit:dB][symbol:lufs_out]out lufs-s",-70,0))) : _,_;

lk2 = lk2_var(3);
lk2_short = lk2_var(0.4);
