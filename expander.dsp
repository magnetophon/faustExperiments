declare name "Expander / Upward Compressor";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

// maximum time in seconds for attack, hold and release
maxRelTime = 1;

process =
  // FFcompressorSC_N_chan(strength,threshold,attack,release,knee,prePost,link,meter,2,sidechain(freq),SCswitch)
  // FFexpanderSC_N_chan(strength,threshold,range,attack,hold,release,knee,prePost,link,meter,5,sidechain(freq),SCswitch)
  FFexpander_N_chan(strength,threshold,range,attack,hold,release,knee,prePost,link,meter,2)
  // , (level(hold,x):peak_expansion_gain_mono(strength,threshold,range,attack,release,knee,prePost)/range*-1)  // for looking at the GR on the scope
;
// example sidechain function
sidechain(freq) = fi.highpass(1,freq);
freq = hslider("SC HP freq", 240, 1, 20000, 1);



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
// x:abs;

//--------------------`(co.)FFexpander_N_chan`-------------------
// feed forward N channel dynamic range expander.
// `FFexpander_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : FFexpander_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) : si.bus(N)
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
  FFexpanderSC_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,meter,N,_,0,0);

//--------------------`(co.)FFexpanderSC_N_chan`-------------------
// feed forward N channel dynamic range expander.
// `FFexpanderSC_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : FFexpanderSC_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) : si.bus(N)
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
// * `SCfunction` : a function that get's placed before the level-detector; needs to have a single input and output
// * `SCswitch` : use either the regular audio inout or the SCsignal as the input for the level detector
// * `SCsignal` : An audiosignal, to be used as the inout for the level detector when SCswitch is 1
//
//------------------------------------------------------------

declare FFexpanderSC_N_chan author "Bart Brouns";
declare FFexpanderSC_N_chan license "GPLv3";

// feed forward expander with sidechain
FFexpanderSC_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) =
  si.bus(N) <:
  ((par(i, N, select2(SCswitch,_,SCsignal):SCfunction)
    : peak_expansion_gain_N_chan(strength,thresh,range,att,hold,rel,knee,prePost,link,N))
  ,si.bus(N))
  : ro.interleave(N,2)
  : par(i,N,(meter:ba.db2linear)*_);




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

sw_group(x)  = env_group(vgroup("[3]", x));
prePost = sw_group(checkbox("[1] slow/fast  [tooltip: Unchecked: log  domain return-to-threshold detector
      Checked: linear return-to-zero detector]")*-1)+1;
SCswitch = sw_group(checkbox("[2] External SideChain  [tooltip: Unchecked: original signal
      Checked: Use external Sidechain]")*-1)+1;

link = env_group(hslider("[4] link [style:knob]
      [tooltip: 0 means all channels get individual gain reduction, 1 means they all get the same gain reduction]",
                         1, 0, 1, 0.01));

// meter upward
// SC function
// lin / log




//--------------------`(co.)FFcompressorSC_N_chan`-------------------
// feed forward N channel dynamic range compressor.
// `FFcompressorSC_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : FFcompressorSC_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) : si.bus(N)
// ```
//


// Where:
//
// * `strength`: strength of the compression (0 = no compression, 1 means hard limiting, >1 means over-compression)
// * `thresh`: dB level threshold above which compression kicks in
// * `att`: attack time = time constant (sec) when level & compression going up
// * `rel`: release time = time constant (sec) coming out of compression
// * `knee`: a gradual increase in gain reduction around the threshold:
// Below thresh-(knee/2) there is no gain reduction,
// above thresh+(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-threshold detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `meter`: a gain reduction meter. It can be implemented like so:
// meter = _<:(_, (ba.linear2db:max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;
// * `N`: the number of channels of the compressor
// * `SCfunction` : a function that get's placed before the level-detector; needs to have a single input and output
// * `SCswitch` : use either the regular audio inout or the SCsignal as the input for the level detector
// * `SCsignal` : An audiosignal, to be used as the inout for the level detector when SCswitch is 1
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

declare FFcompressorSC_N_chan author "Bart Brouns";
declare FFcompressorSC_N_chan license "GPLv3";

// feed forward compressor
FFcompressorSC_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) =
  si.bus(N) <: (
  (par(i, N, select2(SCswitch,_,SCsignal):SCfunction):peak_compression_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N))
 ,si.bus(N)
) : ro.interleave(N,2) : par(i,N,meter*_);

//--------------------`(co.)FBcompressorSC_N_chan`-------------------
// feed back N channel dynamic range compressor.
// `FBcompressorSC_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : FBcompressorSC_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) : si.bus(N)
// ```
//
// Where:
//
// * `strength`: strength of the compression (0 = no compression, 1 means hard limiting, >1 means over-compression)
// * `thresh`: dB level threshold above which compression kicks in
// * `att`: attack time = time constant (sec) when level & compression going up
// * `rel`: release time = time constant (sec) coming out of compression
// * `knee`: a gradual increase in gain reduction around the threshold:
// Below thresh-(knee/2) there is no gain reduction,
// above thresh+(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-threshold detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `meter`: a gain reduction meter. It can be implemented like so:
// `meter = _ <: (_,(ba.linear2db:max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;`
// or it can be omitted by defining `meter = _;`.
// * `N`: the number of channels of the compressor
// * `SCfunction` : a function that get's placed before the level-detector; needs to have a single input and output
// * `SCswitch` : use either the regular audio inout or the SCsignal as the input for the level detector
// * `SCsignal` : An audiosignal, to be used as the inout for the level detector when SCswitch is 1
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

declare FBcompressorSC_N_chan author "Bart Brouns";
declare FBcompressorSC_N_chan license "GPLv3";

FBcompressorSC_N_chan(strength,thresh,att,rel,knee,prePost,link,meter,N,SCfunction,SCswitch,SCsignal) =
  ( (par(i, N, select2(SCswitch,_,SCsignal):SCfunction):
     peak_compression_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N)),si.bus(N) : (ro.interleave(N,2) : par(i,N,meter*_))) ~ si.bus(N);

//--------------------`(co.)FBFFcompressorSC_N_chan`-------------------
// feed forward / feed back N channel dynamic range compressor.
// The feedback part has a much higher strength, so they end up sounding similar.
// `FBFFcompressorSC_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : FBFFcompressorSC_N_chan(strength,thresh,att,rel,knee,prePost,link,FBFF,meter,N) : si.bus(N)
// ```
//
// Where:
//
// * `strength`: strength of the compression (0 = no compression, 1 means hard limiting, >1 means over-compression)
// * `thresh`: dB level threshold above which compression kicks in
// * `att`: attack time = time constant (sec) when level & compression going up
// * `rel`: release time = time constant (sec) coming out of compression
// * `knee`: a gradual increase in gain reduction around the threshold:
// Below thresh-(knee/2) there is no gain reduction,
// above thresh+(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-threshold detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `FBFF`: fade between feed forward (0) and feed back (1) compression.
// * `meter`: a gain reduction meter. It can be implemented like so:
// `meter = _<:(_,(ba.linear2db:max(maxGR):meter_group((hbargraph("[1][unit:dB][tooltip: gain reduction in dB]", maxGR, 0))))):attach;`
// * `N`: the number of channels of the compressor
// * `SCfunction` : a function that get's placed before the level-detector; needs to have a single input and output
// * `SCswitch` : use either the regular audio inout or the SCsignal as the input for the level detector
// * `SCsignal` : An audiosignal, to be used as the inout for the level detector when SCswitch is 1
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

declare FBFFcompressorSC_N_chan author "Bart Brouns";
declare FBFFcompressorSC_N_chan license "GPLv3";

FBFFcompressorSC_N_chan(strength,thresh,att,rel,knee,prePost,link,FBFF,meter,N) =
  si.bus(N) <: si.bus(N*2) :
  (
    ((par(i,2,peak_compression_gain_N_chan(strength*(1+((i ==0)*2)),thresh,att,rel,knee,prePost,link,N)) : ro.interleave(N,2) : par(i,N,it.interpolate_linear(FBFF))),si.bus(N))
    : (ro.interleave(N,2) : par(i,N,meter*_))
  )
  ~ si.bus(N);

peak_compression_gain_mono(strength,thresh,att,rel,knee,prePost) =
  abs : ba.bypass1(prePost,si.onePoleSwitching(att,rel)) : ba.linear2db : gain_computer(strength,thresh,knee) : ba.bypass1((prePost !=1),si.onePoleSwitching(rel,att)) : ba.db2linear
with {
  gain_computer(strength,thresh,knee,level) =
    select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
            0,
            ((level-thresh+(knee/2)) : pow(2)/(2*max(ma.EPSILON,knee))),
            (level-thresh))
    : max(0)*-strength;
};


//--------------------`(co.)peak_compression_gain_N_chan`-------------------
// N channel dynamic range compressor gain computer.
// `peak_compression_gain_N_chan` is a standard Faust function.
//
// #### Usage
//
// ```
// si.bus(N) : peak_compression_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N) : si.bus(N)
// ```
//
// Where:
//
// * `strength`: strength of the compression (0 = no compression, 1 means hard limiting, >1 means over-compression)
// * `thresh`: dB level threshold above which compression kicks in
// * `att`: attack time = time constant (sec) when level & compression going up
// * `rel`: release time = time constant (sec) coming out of compression
// * `knee`: a gradual increase in gain reduction around the threshold:
// Below thresh-(knee/2) there is no gain reduction,
// above thresh+(knee/2) there is the same gain reduction as without a knee,
// and in between there is a gradual increase in gain reduction.
// * `prePost`: places the level detector either at the input or after the gain computer;
// this turns it from a linear return-to-zero detector into a log  domain return-to-threshold detector
// * `link`: the amount of linkage between the channels. 0 = each channel is independent, 1 = all channels have the same amount of gain reduction
// * `N`: the number of channels of the compressor
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

declare peak_compression_gain_N_chan author "Bart Brouns";
declare peak_compression_gain_N_chan license "GPLv3";

// generalise compression gains for N channels.
// first we define a mono version:
peak_compression_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,1) =
  peak_compression_gain_mono(strength,thresh,att,rel,knee,prePost);

// The actual N-channels version:
// Calculate the maximum gain reduction of N channels,
// and then crossfade between that and each channel's own gain reduction,
// to link/unlink channels
peak_compression_gain_N_chan(strength,thresh,att,rel,knee,prePost,link,N) =
  par(i, N, peak_compression_gain_mono(strength,thresh,att,rel,knee,prePost))
  <: (si.bus(N),(ba.parallelMin(N) <: si.bus(N))) : ro.interleave(N,2) : par(i,N,(it.interpolate_linear(link)));
