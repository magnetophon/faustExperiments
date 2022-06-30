declare name "Expander";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

process(x) =
  // process =
  x
, expander(strength,threshold,range,attack,hold,release,x)
  //
  // , (expander_gain_computer(ratio,threshold,range,level(x,hold)):si.lag_ud(attack,release)/range*-1)

, (level(x,hold):peak_expansion_gain_mono(strength,threshold,range,attack,release,knee,prePost)/range*-1)

  // , (x:peak_expansion_gain_mono(strength,threshold,attack,release,knee,prePost)/range*-1)
  // , (((level(x,hold)>(threshold-(knee/2)))+(level(x,hold)>(threshold+(knee/2))))/2)
  // , (((x>(threshold-(knee/2)))+(x>(threshold+(knee/2))))/2)
;

// x = (os.lf_saw(100)-1)/2;
// x = os.lf_saw(100)*-1;
// x = os.lf_saw(1)*makeupgain;

// expander(x,y) =
// peak_expansion_gain_mono(ratio,thresh,range,att,rel,knee,prePost);

expander_gain_computer(ratio,thresh,range,level) =
  (level-thresh):min(0)
                 * abs(ratio) : max(range)
                                * (-1+(2*(ratio>0)))
;

OKexpander_gain_computer(ratio,thresh,range,level) =
  (
    // overThreshold * overThresholdGain // always 0
    // ,
    inRange * inRangeGain
  , underRange * underRangeGain
  ):>_
with {
  overThreshold = level >= thresh;
  overThresholdGain = 0;
  inRange = (level < thresh) * (level > bottom);
  inRangeGain = level-thresh;
  underRange = level <= bottom;
  underRangeGain = range;
  bottom = (thresh+range);
};


maxRelTime = 0.1;
expander(strength,thresh,range,attack,hold,release,x) =
  (
    // expander_gain_computer(ratio,thresh,range,level(x,hold))
    // :si.lag_ud(attack,release)
    peak_expansion_gain_mono(strength,thresh,range,attack,release,knee,prePost,level(x,hold))
    :ba.db2linear
  ) *x;

SCexpander(strength,thresh,knee,att,rel,SC,x) = (expander_gain_computer(strength,thresh,knee,level(att,rel,SC)):ba.db2linear):meter*x;

level(x,hold) =
  // x;
  x:abs:ba.linear2db:ba.slidingMax(hold*ma.SR,192000);
// x:abs:ba.linear2db;
comp_group(x) = vgroup("COMPRESSOR  [tooltip: Reference: http://en.wikipedia.org/wiki/Dynamic_range_compression]", x);

meter_group(x)  = comp_group(vgroup("[0]", x));
knob_group(x)  = comp_group(hgroup("[1]", x));

checkbox_group(x)  = meter_group(hgroup("[0]", x));

cbp = checkbox_group(checkbox("[0] Bypass  [tooltip: When this is checked, the compressor has no effect]"));
maxGR = -100;
meter = _<:(_, (ba.linear2db:max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;

ctl_group(x)  = knob_group(hgroup("[3] Compression Control", x));

strength = ctl_group(hslider("[0] Strength [style:knob]
      [tooltip: A compression Strength of 0 means no gain reduction and 1 means full gain reduction]",
                             8, -100, 100, 0.1));

ratio = ctl_group(hslider("[0] Ratio [style:knob]
      [tooltip: A compression Strength of 0 means no gain reduction and 1 means full gain reduction]",
                          8, -100, 100, 0.1));
duck_strength =
  ctl_group(hslider("[-1] Duck Strength [style:knob]
          [tooltip: A compression Strength of 0 means no gain reduction and 1 means full gain reduction]",
                    1, 0, 8, 0.01));

expand_strength =
  ctl_group(hslider("[0] Expand Strength [style:knob]
          [tooltip: A compression Strength of 0 means no gain reduction and 1 means full gain reduction]",
                    1, 0, 8, 0.01));

threshold = ctl_group(hslider("[1] Threshold [unit:dB] [style:knob]
      [tooltip: When the signal level exceeds the Threshold (in dB), its level is compressed according to the Strength]",
                              maxGR, maxGR, 10, 0.1));

range = ctl_group(hslider("[1] Range [unit:dB] [style:knob]
      [tooltip: When the signal level exceeds the Threshold (in dB), its level is compressed according to the Strength]",
                          maxGR*0.3, maxGR, 0, 0.1));

knee = ctl_group(hslider("[2] Knee [unit:dB] [style:knob]
      [tooltip: soft knee amount in dB]",
                         6, 0, 30, 0.1));

HPfreq =
  ctl_group(hslider("[3] HP freq [scale:log] [style:knob]
          [tooltip: cutoff frequency of the sidechain fi.highpass filter]",
                    20, 20, 10000, 1));

LPfreq =
  ctl_group(hslider("[4] LP freq [scale:log] [style:knob]
          [tooltip: cutoff frequency of the sidechain fi.lowpass filter]",
                    10000, 20, 10000, 1));

SClisten = knob_group(checkbox("SC"));

env_group(x)  = knob_group(hgroup("[4] Compression Response", x));

attack = env_group(hslider("[1] Attack [unit:ms] [style:knob] [scale:log]
      [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')]",
                           2, 0.001, 1000, 0.1)-0.001) : *(0.001) :max(0) :hbargraph("attack", 0, 1);
hold = env_group(hslider("[1] Hold [unit:ms] [style:knob] [scale:log]
      [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')]",
                         30, 0.001, 1000, 0.1)-0.001) : *(0.001) :max(0) :hbargraph("hold", 0, 1);
// The actual attack value is 0.1 smaller than the one displayed.
// This is done for hard limiting:
// You need 0 attack for that, but a log scale starting at 0 is useless

release = env_group(hslider("[2] Release [unit:ms] [style: knob] [scale:log]
      [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')]",
                            42, 0.001, maxRelTime*1000, 0.1)-0.001) : *(0.001) : max(0): hbargraph("release", 0, 1);

prePost = env_group(checkbox("[3] slow/fast  [tooltip: Unchecked: log  domain return-to-threshold detector
      Checked: linear return-to-fi.zero detector]")*-1)+1;

link = env_group(hslider("[4] link [style:knob]
      [tooltip: 0 means all channels get individual gain reduction, 1 means they all get the same gain reduction]",
                         1, 0, 1, 0.01));

FBFF = env_group(hslider("[5] feed-back/forward [style:knob]
      [tooltip: fade between a feedback and a feed forward compressor design]",
                         1, 0, 1, 0.01));

lim_group(x)  = knob_group(hgroup("[5] Limiter [tooltip: It's release time is the minimum of the attack and release of the compressor,
      and it's knee is half that of the compressor]", x));

thresholdLim = lim_group(hslider("[9] Threshold [unit:dB] [style:knob]
      [tooltip: The signal level never exceeds this threshold]",
                                 0, -30, 10, 0.1));

makeupgain = comp_group(hslider("[6] Makeup Gain [unit:dB]
      [tooltip: The compressed-signal input level is increased by this amount (in dB) to make up for the level lost due to compression]",
                                0, 0, maxGR*-1, 0.1)) : ba.db2linear;
//--------------------`(co.)peak_expansion_gain_N_chan`-------------------
// N channel dynamic range expander gain computer.
// `peak_expansion_gain_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : peak_expansion_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N) : si.bus(N)
// ```
//
// Where:
//
// * `strength`: strength of the expansion (0 = no expansion, 1 means hard limiting, >1 means over-expansion)
// * `thresh`: dB level threshold above which expansion kicks in
// * `att`: attack time = time constant (sec) when level & expansion going up
// * `rel`: release time = time constant (sec) coming out of expansion
// * `knee`: a gradual increase in gain reduction around the threshold:
// Below thresh-(knee/2) there is no gain reduction,
// above thresh+(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-threshold detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `N`: the number of channels of the expander
//
// It uses a strength parameter instead of the traditional ratio, in order to be able to
// function as a hard limiter.
// For that you'd need a ratio of infinity:1, and you cannot express that in Faust.
//
// Sometimes even bigger ratios are useful:
// for example a group recording where one instrument is recorded with both a close microphone and a room microphone,
// and the instrument is loud enough in the room mic when playing loud, but you want to boost it when it is playing soft.
//
// #### References
//
// * <http://en.wikipedia.org/wiki/Dynamic_range_expansion>
// * Digital Dynamic Range Compressor Design
// A Tutorial and Analysis
// DIMITRIOS GIANNOULIS (Dimitrios.Giannoulis@eecs.qmul.ac.uk)
// MICHAEL MASSBERG (michael@massberg.org)
// AND JOSHUA D. REISS (josh.reiss@eecs.qmul.ac.uk)
//------------------------------------------------------------

declare peak_expansion_gain_N_chan author "Bart Brouns";
declare peak_expansion_gain_N_chan license "GPLv3";

// generalise expansion gains for N channels.
// first we define a mono version:
peak_expansion_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,1) =
  peak_expansion_gain_mono(strength,thresh,att,rel,knee,prePost);

// The actual N-channels version:
// Calculate the maximum gain reduction of N channels,
// and then crossfade between that and each channel's own gain reduction,
// to link/unlink channels
peak_expansion_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N) =
  par(i, N, peak_expansion_gain_mono(strength,thresh,att,rel,knee,prePost))
  <: (si.bus(N),(ba.parallelMin(N) <: si.bus(N))) : ro.interleave(N,2) : par(i,N,(it.interpolate_linear(link)));

// https://www.jstage.jst.go.jp/article/ast1980/16/6/16_6_353/_article/-char/ja/

// note: si.lag_ud has a bug where if you compile with standard precision,
// down is 0 and prePost is 1, you go into infinite GR and stay there
// peak_expansion_gain_mono(strength,thresh,range,att,rel,knee,prePost,level(x,hold))
peak_expansion_gain_mono(strength,thresh,range,attack,release,knee,prePost,level) =
  level:ba.bypass1(prePost,si.lag_ud(attack,release)) : gain_computer(strength,thresh,range,knee) : ba.bypass1((prePost !=1),si.lag_ud(att,rel))
with {
  gain_computer(strength,thresh,range,knee,level) =
    // , (((level(x,hold)>(threshold-(knee/2)))+(level(x,hold)>(threshold+(knee/2))))/2)
    ( select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2)))
             , (level-thresh)
               // , ((level-thresh+(knee/2)) : pow(2)/(2*max(ma.EPSILON,knee)))
             , ((level-thresh-(knee/2)):pow(2) /(min(ma.EPSILON,knee*-2)))
             , 0
             )  *abs(strength):max(range)
                               * (-1+(2*(strength>0)))
    ):max(range);
  att = select2((strength>0),release,attack);
  rel = select2((strength>0),attack,release);
};

//--------------------`(co.)FFexpander_N_chan`-------------------
// feed forward N channel dynamic range expander.
// `FFexpander_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : FFexpander_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N) : si.bus(N)
// ```
//
// Where:
//
// * `strength`: strength of the expansion (0 = no expansion, 1 means hard limiting, >1 means over-expansion)
// * `thresh`: dB level threshold above which expansion kicks in
// * `att`: attack time = time constant (sec) when level & expansion going up
// * `rel`: release time = time constant (sec) coming out of expansion
// * `knee`: a gradual increase in gain reduction around the threshold:
// Below thresh-(knee/2) there is no gain reduction,
// above thresh+(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-threshold detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `meter`: a gain reduction meter. It can be implemented like so:
// meter = _<:(_, (ba.linear2db:max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;
// * `N`: the number of channels of the expander
//
// It uses a strength parameter instead of the traditional ratio, in order to be able to
// function as a hard limiter.
// For that you'd need a ratio of infinity:1, and you cannot express that in Faust.
//
// Sometimes even bigger ratios are useful:
// for example a group recording where one instrument is recorded with both a close microphone and a room microphone,
// and the instrument is loud enough in the room mic when playing loud, but you want to boost it when it is playing soft.
//
// #### References
//
// * <http://en.wikipedia.org/wiki/Dynamic_range_compression>
// * Digital Dynamic Range Compressor Design
// A Tutorial and Analysis
// DIMITRIOS GIANNOULIS (Dimitrios.Giannoulis@eecs.qmul.ac.uk)
// MICHAEL MASSBERG (michael@massberg.org)
// AND JOSHUA D. REISS (josh.reiss@eecs.qmul.ac.uk)
//------------------------------------------------------------

declare FFexpander_N_chan author "Bart Brouns";
declare FFexpander_N_chan license "GPLv3";

// feed forward expander
FFexpander_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N) =
  si.bus(N) <: (peak_expansion_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N),si.bus(N)) : ro.interleave(N,2) : par(i,N,meter*_);

// trummerschlunk
// hi guys,
// I tried before on the mailinglist, but I am still looking for an expander in faust. There is a gate and several excellent compressors, but I am missing a dynamic processor that applies a ratio below a threshold.
// I guess, for some of you, it wouldn't be so hard to modify one of the compressors in the co. library, but I can't figure it out myself.
// According to peak_compression_gain_mono the parameters of could be: strength, thresh, att, rel, knee, prePost plus range as a parameter for maximum attenuation.
// Attached is a picture I found on the net, that describes it precisely.
// (oh and strenght >1 could result in a so called upward-compressor, which sounds really nice sometimes)
// Thank you so much,
// Klaus
