declare name "Expander / Upward Compressor";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

// maximum time in seconds for attack, hold and release
maxRelTime = 1;

process(x,y) =
  FFexpander_N_chan(strength,threshold,range,attack,hold,release,knee,prePost,link,meter,2,x,y)
  // , (level(hold,x):peak_expansion_gain_mono(strength,threshold,range,attack,release,knee,prePost)/range*-1)
;
//--------------------`(co.)peak_expansion_gain_N_chan`-------------------
// N channel dynamic range expander gain computer.
// `peak_expansion_gain_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : peak_expansion_gain_N_chan(strength,thresh,range,att,rel,knee,prePost,link,N) : si.bus(N)
// ```
//
// Where:
//
// * `strength`: strength of the expansion (0 = no expansion, 100 means gating, <1 means upward compression)
// * `thresh`: dB level threshold below which expansion kicks in
// * `range`: maximum amount of expansion in dB
// * `att`: attack time = time constant (sec) coming out of expansion
// * `hold` : hold time (sec)
// * `rel`: release time = time constant (sec) going into expansion
// * `knee`: a gradual increase in gain reduction around the threshold:
// Above thresh+(knee/2) there is no gain reduction,
// below thresh-(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-range detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `N`: the number of channels of the expander
//
//------------------------------------------------------------

declare peak_expansion_gain_N_chan author "Bart Brouns";
declare peak_expansion_gain_N_chan license "GPLv3";

// generalise expansion gains for N channels.
// first we define a mono version:
peak_expansion_gain_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,1) =
  level(hold) : peak_expansion_gain_mono(strength,thresh,range,att,rel,knee,prePost);

// The actual N-channels version:
// Calculate the maximum gain reduction of N channels,
// and then crossfade between that and each channel's own gain reduction,
// to link/unlink channels
peak_expansion_gain_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,N) =
  par(i, N, level(hold) :peak_expansion_gain_mono(strength,thresh,range,att,rel,knee,prePost))

  <: (si.bus(N),(ba.parallelMax(N) <: si.bus(N))) : ro.interleave(N,2) : par(i,N,(it.interpolate_linear(link)));

peak_expansion_gain_mono(strength,thresh,range,attack,release,knee,prePost,level) =
  level:ba.bypass1(prePost,si.lag_ud(attack,release)) :ba.linear2db : gain_computer(strength,thresh,range,knee) : ba.bypass1((prePost !=1),si.lag_ud(att,rel))
with {
  gain_computer(strength,thresh,range,knee,level) =
    ( select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2)))
             , (level-thresh)
             , ((level-thresh-(knee/2)):pow(2) /(min(ma.EPSILON,knee*-2)))
             , 0
             )  *abs(strength):max(range)
                               * (-1+(2*(strength>0)))
    );
  att = select2((strength>0),release,attack);
  rel = select2((strength>0),attack,release);
};

level(hold,x) =
  x:abs:ba.slidingMax(hold*ma.SR,192000*maxRelTime);

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
// * `strength`: strength of the expansion (0 = no expansion, 100 means gating, <1 means upward compression)
// * `thresh`: dB level threshold below which expansion kicks in
// * `range`: maximum amount of expansion in dB
// * `att`: attack time = time constant (sec) coming out of expansion
// * `hold` : hold time
// * `rel`: release time = time constant (sec) going into expansion
// * `knee`: a gradual increase in gain reduction around the threshold:
// Above thresh+(knee/2) there is no gain reduction,
// below thresh-(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-range detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `meter`: a gain reduction meter. It can be implemented like so:
// meter = _<:(_, (ba.linear2db:max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;
// * `N`: the number of channels of the expander
//
//------------------------------------------------------------

declare FFexpander_N_chan author "Bart Brouns";
declare FFexpander_N_chan license "GPLv3";

// feed forward expander
FFexpander_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,meter,N) =
  si.bus(N) <: (peak_expansion_gain_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,N),si.bus(N)) : ro.interleave(N,2) : par(i,N,(meter:ba.db2linear)*_);

//---------------------------------- GUI --------------------------------------
expander_group(x) = vgroup("Expander / Upward Compressor", x);

meter_group(x)  = expander_group(vgroup("[0]", x));
knob_group(x)  = expander_group(hgroup("[1]", x));

checkbox_group(x)  = meter_group(hgroup("[0]", x));

maxGR = -100;
meter = _<:(_, (max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;

ctl_group(x)  = knob_group(hgroup("[3] Compression Control", x));

threshold = ctl_group(hslider("[0] Threshold [unit:dB] [style:knob]
      [tooltip: When the signal level exceeds the Threshold (in dB), its level is compressed according to the Strength]",
                              maxGR, maxGR, 10, 0.1));

strength = ctl_group(hslider("[1] Strength [style:knob]
      [tooltip: A compression Strength of 0 means no gain reduction and 1 means full gain reduction]",
                             8, -100, 100, 0.1));

range = ctl_group(hslider("[2] Range [unit:dB] [style:knob]
      [tooltip: When the signal level exceeds the Threshold (in dB), its level is compressed according to the Strength]",
                          maxGR*0.3, maxGR, 0, 0.1));

knee = ctl_group(hslider("[3] Knee [unit:dB] [style:knob]
      [tooltip: soft knee amount in dB]",
                         6, 0, 30, 0.1));

env_group(x)  = knob_group(hgroup("[4] Compression Response", x));

attack = env_group(hslider("[1] Attack [unit:ms] [style:knob] [scale:log]
      [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')]",
                           2, 0.001, 1000, 0.1)-0.001) : *(0.001) :max(0);
hold = env_group(hslider("[2] Hold [unit:ms] [style:knob] [scale:log]
      [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')]",
                         30, 0.001, 1000, 0.1)-0.001) : *(0.001) :max(0);
// The actual attack value is 0.1 smaller than the one displayed.
// This is done for hard limiting:
// You need 0 attack for that, but a log scale starting at 0 is useless

release = env_group(hslider("[3] Release [unit:ms] [style: knob] [scale:log]
      [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')]",
                            42, 0.001, maxRelTime*1000, 0.1)-0.001) : *(0.001) : max(0);

prePost = env_group(checkbox("[3] slow/fast  [tooltip: Unchecked: log  domain return-to-threshold detector
      Checked: linear return-to-zero detector]")*-1)+1;

link = env_group(hslider("[4] link [style:knob]
      [tooltip: 0 means all channels get individual gain reduction, 1 means they all get the same gain reduction]",
                         1, 0, 1, 0.01));
