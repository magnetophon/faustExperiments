declare name "simpleCompressor";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2025 - 2025, Bart Brouns";


// TODO:
// -add lookahead att in hold
// - make simple:
//   - input gain
//   - output gain
//
//  separate limiter detector (no smoothing needed?), that sends the gain:
//  - back to the input of the DC detector
//  - back to the input of the compressor detector
//  - and forward to the lookahead smoother
//
//  use prevDiv to set DC attack to 0 and release to less




import("stdfaust.lib");


process =
  hgroup("", 
         vgroup("DCcompensator", DCcompensator)
         :hgroup("simpleCompressor", simpleCompressor) 
         : vgroup("limiter", limiter))~_
                                       :(!,_,_,_);

limiter(inputGain,compGain,offset,x) =
  max(maxAmount,rawGain*strength)
  : ba.db2linear
    // smoothedGain
, (inputGain@lookaheadSamples*smoothedGain*(x@lookaheadSamples-smoothedOffset))
  // , (ba.linear2db(smoothedGain)+ ba.linear2db(compGain):ba.db2linear)
, smoothedGain
, smoothedOffset
  // , ba.db2linear(compGain)
with {
  // TODO: make att time SR independant?
  lookaheadSamples = 64;
  rawGain =
    smootherARorder(orderHold, orderHold, orderHold, 0, timeHold, abs((x-offset)*inputGain*ba.db2linear(compGain)))
    :ba.linear2db
    : gain_computer(1,thresh,knee)
    : smootherARorder(maxOrder, orderRelLim, 4, timeRelLim*pre, 0)
  ;
  smoothedGain =
    rawGain+compGain
    : ba.db2linear
    :ba.slidingMin(lookaheadSamples,lookaheadSamples)
     // : smootherARorder(maxOrder, 4, 4, timeRelLim, lookaheadSamples/ma.SR)
    : smootherARorder(maxOrder, 4, 4, timeRelLim*(1-pre), lookaheadSamples/ma.SR)
  ;
  smoothedOffset =
    ((offset*(offset>0))
     :ba.slidingMax(lookaheadSamples,lookaheadSamples)
     : smootherARorder(maxOrder, 4, 4, lookaheadSamples/ma.SR, lookaheadSamples/ma.SR)
    ) 
    +
    ((offset*(offset<0))
     :ba.slidingMin(lookaheadSamples,lookaheadSamples)
     : smootherARorder(maxOrder, 4, 4, 0,lookaheadSamples/ma.SR)
    )
  ;
  pre = checkbox("pre");
  maxAmount = hslider("max amount[unit:dB]", -3, -6, 0, 0.01);
};

simpleCompressor(limGain,offset,x) =
  // x ,
  // *gain
  // :aa.softclipQuadratic2
  // :fi.svf.bell(HSfreq,HSq,HSgain*checkbox("HS"))
  // *0.5
  inputGain 
, gain
, offset
, x
  // , abs(inputSignal)
  // , attEnv(abs(x))
with {
  // envFollow = abs(inputSignal):holdEnv
  // : attRelEnv;
  gain = abs((x-offset)*inputGain)
         : ((
             (_,(holdEnv
                 : ba.linear2db
                 : gain_computer(strength,thresh,knee)))
             : attRelEnv)~(_<:(_,_))
                          :(!,_)
           )
         : mainGroup(hbargraph("GR", -24, 0))
           // : ba.db2linear
  ;
  // inputSignal = x*inputGain
  // :fi.highpass(3,hpFreq)
  // ;

  HSfreq = 20000;
  HSq = 0.343;
  HSgain =
    9;
  // envFollow = abs(x):attEnv:holdEnv:relEnv;
  // holdEnv(prevDiv,x) = smootherARorder(orderHold, orderHold, orderHold, 0, timeHold, x);
  holdEnv(prevDiv,x) = smootherARorder(orderHold, orderHold, orderHold, 0,
                                       it.interpolate_linear(prevDiv,timeHold,timeHold*slowGroup(hslider("mult hold", 0.1, 0, 1, 0.001)))
                                       , x*limGain);
  // attRelEnv(x) = smootherARorder(maxOrder, orderAtt, orderRel, timeAtt, timeRel, x);
  attRelEnv(prevDiv,x) =
    smootherARorder(maxOrder, orderRel, orderAtt, it.interpolate_linear(prevDiv,timeRel,timeRel*slowGroup(hslider("mult rel", 0.1, 0, 1, 0.001))),  timeAtt, x)
    <:(
    (
      // select2(checkbox("sel") ,
      // , ((x-_)/max(x,ma.EPSILON))
      // ((x-slow(x))/max(x,ma.EPSILON))
      // , ((slow(x)-x)/max(x,ma.EPSILON))

      slowGroup(select2(checkbox("sel")),
                
                (
                  ((slow(ba.db2linear(_))-ba.db2linear(_))/max(ba.db2linear(_),ma.EPSILON))
                  :ba.linear2db
                )

                ,

                  (
                    ((slow(ba.db2linear(x))-ba.db2linear(x))/max(ba.db2linear(x),ma.EPSILON))
                    :ba.linear2db
                  )
               )

      // ,
      // (
      // ((slow(ba.db2linear(x))-ba.db2linear(x))/max(ba.db2linear(x),ma.EPSILON))
      // :ba.linear2db
      // )
      // )
      :max(0):min(1)
      :slowGroup(hbargraph("div", 0, 1))
    )
  , _
  )
  ;
  // TODO: this can be an env follower (if we keep the rel at 0)
  // have to flip the input first
  slow(x) = slowGroup(smootherARorder(2, orderRelSlow, orderAttSlow, timeRelSlow, timeAttSlow, x));
  // holdEnv = smootherARorder(maxOrder,orderAtt, orderHold, timeAtt, timeHold, abs(x));
  attEnv(x) = smootherARorder(maxOrder, orderAtt, orderAttHold, timeAtt, timeAttHold, x);


  relEnv(x) = smootherARorder(maxOrder,1, orderRel, 0, timeRel, x);
};


///////////////////////////////////////////////////////////////////////////////
//                               groups                                     //
///////////////////////////////////////////////////////////////////////////////
mainGroup(x) = vgroup("main", x);
slowGroup(x) = vgroup("slow", x);
///////////////////////////////////////////////////////////////////////////////
//                               GUI                                     //
///////////////////////////////////////////////////////////////////////////////
maxOrder = 4;
orderHold = 16;
// hslider("order hold", maxOrder, 1, maxOrder, 1);
orderAtt =
  4;
// hslider("order att", 4, 1, maxOrder, 1);
orderAttSlow =
  // 2;
  hslider("order att slow", 1, 1, maxOrder, 1);
orderRelSlow =
  // 2;
  hslider("order rel slow", 1, 1, maxOrder, 1);

orderAttHold = hslider("order att hold", 4, 1, maxOrder, 1);
orderRel = 2;
// hslider("order rel", 2, 1, maxOrder, 1);
orderRelLim =
  // 2;
  hslider("order rel lim", 2, 1, maxOrder, 1);
timeHold =
  // timeRel*0.333;
  mainGroup(hslider("hold time[scale:log]", 0.11, 0.001, 0.2, 0.001));
timeAtt = mainGroup(hslider("att time[scale:log]", 0.001, 0.001, 0.05, 0.001)-0.001);
timeAttSlow = hslider("att time slow[scale:log]", 0.2, 0.001, 1, 0.001)-0.001;
timeRelSlow = hslider("rel time slow[scale:log]", 0.001, 0.001, 0.5, 0.001)-0.001;
timeAttHold = hslider("att hold time[scale:log]", 0.001, 0.001, 0.1, 0.001)-0.001;
timeRel = mainGroup(hslider("rel time[scale:log]", 0.15, 0.013, 0.5, 0.001));
timeRelLim = mainGroup(hslider("rel time lim[scale:log]", 0.013, 0.001, 0.1, 0.001));

inputGain = mainGroup(hslider("[01]input gain[unit:dB]", 0, 0, 30, 0.1):ba.db2linear:si.smoo);
hpFreq = hslider("high pass freq", 20, 2, 40, 1);
strength =
  // it.remap(0, 0.5, 0, 1,oneKnob:min(0.5));
  mainGroup(hslider("[02]strength[unit:%]", 100, 0, 100, 1)) * 0.01;
thresh = mainGroup(hslider("[14]threshold[unit:dB]",-1,-10,0,0.1));
knee =
  mainGroup(hslider("[17]knee[unit:dB]",1,0,72,0.1));
// it.remap(0.5, 1, 12, 0,oneKnob:max(0.5));

orderDCatt =
  // 1;
  hslider("order att DC", 4, 1, maxOrder, 1);
orderDCrel =
  // 4;
  hslider("order rel DC", 4, 1, maxOrder, 1);
timeDCatt =
  // 0;
  hslider("att time DC[scale:log]", 0.006, 0.001, 0.10, 0.001)-0.001;
timeDCrel =
  // 0.3;
  hslider("rel time DC[scale:log]", 0.069, 0.013, 0.5, 0.001);

gain_computer(strength,thresh,knee,level) =
  select3((level>(thresh-(knee/2)))+(level>(thresh+(knee/2))),
          0,
          ((level-thresh+(knee/2)) : pow(2)/(2*max(ma.EPSILON,knee))),
          (level-thresh))
  : max(0)*-strength;

///////////////////////////////////////////////////////////////////////////////
//                               smoothers                                   //
///////////////////////////////////////////////////////////////////////////////

// smoother adapted from Dario Sanfilippo
// https://github.com/dariosanfilippo/limiterStereo/blob/da1c38cc393f08b5dd79e56ffd4e6256af07a708/limiterStereo.dsp#L90-L101
//
// fixed order
smoother(order, att, rel, xx) =
  smootherOrder(order,order, att, rel, xx);

smootherOrder(maxOrder,order, att, rel, xx) =
  smootherARorder(maxOrder,order, order, att, rel, xx);

smootherARorder(maxOrder,orderAtt, orderRel, att, rel, xx) =
  xx : seq(i, maxOrder, loop(i) ~ _)
with {
  loop(i,fb, x) = coeff(i) * fb + (1.0 - coeff(i)) * x
  with {
  cutoffCorrection(order) = 1.0 / sqrt(pow(2.0, 1.0 / order) - 1.0);
  coeff(i) =
    ba.if(x > fb, attCoeff(i), relCoeff(i) );
  attCoeff(i) =
    exp(-TWOPIT * cutoffCorrection(orderAtt) / max(ma.EPSILON, att))
    * (i<orderAtt);
  relCoeff(i) =
    exp(-TWOPIT * cutoffCorrection(orderRel) / max(ma.EPSILON, rel))
    * (i<orderRel);
  TWOPIT = 2 * ma.PI * ma.T;
};
};

///////////////////////////////////////////////////////////////////////////////
//                            DC compensator                                 //
///////////////////////////////////////////////////////////////////////////////
DCcompensator(limGain,x) =
  // x
  // , positiveEnv(x)
  // , negativeEnv(x)
  // , offset(x)
  // TODO: we don't need the lim gain in here
  limGain
, (offset(
      x
      // better without?
      *select2(checkbox("lim DC"),1,limGain)
    )*checkbox("DC compensate"))
, x
  // , select2(checkbox("DC compensate")
  // ,x
  // , x - offset(x)
  // )
with {

  positiveEnv(x) =
    // smootherARorder(maxOrder, orderDCrel, orderDCatt, timeDCrel, timeDCatt, x*(x<0));
    smootherARorder(maxOrder, orderDCrel, 1, timeDCrel, 0, x*(x<0))
 ;
 // :smootherARorder(maxOrder, 1, orderDCatt, 0, timeDCatt) ;
 negativeEnv(x) =
   // smootherARorder(maxOrder,  orderDCatt, orderDCrel, timeDCatt, timeDCrel, x*(x>0));
   smootherARorder(maxOrder,  1, orderDCrel, 0, timeDCrel, x*(x>0))
 ;
 // : smootherARorder(maxOrder,orderDCatt, 1, timeDCatt, 0);
 offset(x) =
   ((positiveEnv(x)+negativeEnv(x))*.5)
   : smootherARorder(maxOrder,orderDCatt, orderDCatt, timeDCatt*(limGain>DCthres), timeDCatt*(limGain>DCthres))
     // :absEnv
 ;
 DCthres = hslider("DC thres", 0.999, 0, 1, 0.001);
 absEnv(x) =
   // smootherARorder(maxOrder, 1, orderDCatt, 0, timeDCatt, abs(x))
   smootherARorder(maxOrder,orderDCatt, 1, timeDCatt, 0, abs(x))
   * select2(x>0,-1,1);
};
