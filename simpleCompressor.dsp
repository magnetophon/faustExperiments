declare name "simpleCompressor";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3.0-only";
declare copyright "2025 - 2025, Bart Brouns";

declare stratusId "55631e3a-94f7-42f8-8204-f5c6c11c4a21";
declare stratusVersion "0.1.0";
declare filename "simpleCompressor.dsp";

// TODO:
//
//  Faster strategy?
//  steps:
//  - calculate both gain and offset from "pos-env" and "neg-env"
//
//
// -add lookahead att in hold
// - make simple:
//   - input gain
//   - output gain
//   - mix
//
//  separate limiter detector (no smoothing needed?), that sends the gain:
//  - back to the input of the DC detector
//  - back to the input of the compressor detector
//  - and forward to the lookahead smoother
//
//  use prevDiv to set DC attack to 0 and release to less

// 0=full gui, 1=minimal gui, 2=stratus test app, 3=stratus final app
guiType = 3;

import("stdfaust.lib");


process =
  // compressorGUI(guiType);
  posNegCompressor;

posNegCompressor(x) =
  (PosNeg(x),x)
  : posNegGain
;
posNegGain(positiveEnv,negativeEnv,x) =
  ((x@lookaheadSamples-smoothedOffset*symmetry) * smoothedGain)
  // , offset
  // , positiveEnv,negativeEnv
  // , gain
  // , hold
  // , (smoothedGain/(inputGain*outputGain))
  // , smoothedOffset
  // , abs(x)
  // , (x-offset)
with {
  offset =
    (positiveEnv+negativeEnv) *.5*symmetry;
  hold = max(positiveEnv-offset,((negativeEnv-offset)*-1))
         // *inputGain
  ;
  gain =
    ((
      (
        // _,
        (hold
         : ba.linear2db
           + inputGain
           // we don't need the knee nor the strength, so we can do this much cheaper
           // : gain_computer(strength,thresh,knee)))
         : min(0,thresh-_)))
      // : attRelEnvPrevDiv)~(_<:(_,_))
      // :(!,_)
      : attRelEnv
    )
    )
    : meterGUI(guiType)
      // : ba.db2linear
  ;

  attRelEnv(x) = smootherARorder(maxOrder, orderRel, orderAtt, timeRel, timeAtt, x);

  smoothedGain =
    gain+inputGain+outputGain
    : ba.db2linear
    :ba.slidingMin(lookaheadSamples,lookaheadSamples)
     // : smootherARorder(maxOrder, 4, 4, timeRelLim, lookaheadSamples/ma.SR)
    : smootherARorder(4, 4, 4, lookaheadSamples/ma.SR, lookaheadSamples/ma.SR)
  ;
  smoothedOffset =
    (
      (
        // positiveEnv
        (offset*(offset>0))
        :ba.slidingMax(lookaheadSamples,lookaheadSamples)
        : smootherARorder(4, 4, 4, lookaheadSamples/ma.SR, symmetryRel)
          // : smootherARorder(maxOrder, 4, 4, lookaheadSamples/ma.SR, lookaheadSamples/ma.SR)
      )
      +
      (

        // negativeEnv
        (offset*(offset<0))
        :ba.slidingMin(lookaheadSamples,lookaheadSamples)
        : smootherARorder(4, 4, 4, symmetryRel,lookaheadSamples/ma.SR)
          // : smootherARorder(maxOrder, 4, 4, lookaheadSamples/ma.SR,lookaheadSamples/ma.SR)
      )
    )
    // *0.5
  ;
  pre =
    1;
  // checkbox("pre");
};



compressorGUI(0) =
  hgroup("",
         vgroup("DCcompensator", DCcompensator)
         :vgroup("simpleCompressor", simpleCompressor)
         : vgroup("limiter", limiter))~_
                                       // :(!,_,_,_)
                                       :(!,_)
   ;

   compressorGUI(x) =
     (DCcompensator
      : simpleCompressor
      :  limiter)~_
                  // :(!,_,_,_)
                  :(!,_)
   ;

   lookaheadSamples = 64;
   limiter(inputGain,compGain,offset,x) =
     // max(maxAmount,
     rawGain
     // *FBstrength)
     // : ba.db2linear
     // smoothedGain

     // , ((inputGain*outputGain)@lookaheadSamples*smoothedGain*(x@lookaheadSamples-smoothedOffset))


   , it.interpolate_linear(mix, (x@lookaheadSamples-smoothedOffset),
                           ((inputGain*outputGain)@lookaheadSamples*smoothedGain*(x@lookaheadSamples-smoothedOffset))
                          )

     // , (ba.linear2db(smoothedGain)+ ba.linear2db(compGain):ba.db2linear)
     // , smoothedGain
     // , smoothedOffset
     // , ba.db2linear(compGain)
   with {
     // TODO: make att time SR independant?
     rawGain =
       smootherARorder(orderHold, orderHold, orderHold, 0, timeHoldLim, abs((x-offset)*inputGain*ba.db2linear(compGain)))
       :ba.linear2db
        // we don't need the knee nor the strength, so we can do this much cheaper
        // : gain_computer(1,limThresh,0)
       : min(0,limThresh-_)
         // : smootherARorder(maxOrder, orderRelLim, 4, timeRelLim*pre, 0)
       : smootherARorder(orderRelLim, orderRelLim, orderRelLim, timeRelLim*pre, 0)
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
         // : smootherARorder(maxOrder, 4, 4, lookaheadSamples/ma.SR, timeRelSlow)
        : smootherARorder(maxOrder, 4, 4, lookaheadSamples/ma.SR, lookaheadSamples/ma.SR)
       )
       +
       ((offset*(offset<0))
        :ba.slidingMin(lookaheadSamples,lookaheadSamples)
         // : smootherARorder(maxOrder, 4, 4, timeRelSlow,lookaheadSamples/ma.SR)
        : smootherARorder(maxOrder, 4, 4, lookaheadSamples/ma.SR,lookaheadSamples/ma.SR)
       )
     ;
     pre =
       1;
     // checkbox("pre");
     maxAmount = hslider("max amount[unit:dB]", -30, -30, 0, 0.01);
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
                (
                  // _,
                  (holdEnv
                   : ba.linear2db
                   : gain_computer(strength,thresh,knee)))
                // : attRelEnvPrevDiv)~(_<:(_,_))
                // :(!,_)
                : attRelEnv
              )
              )
            : meterGUI(guiType)
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
     holdEnv(x) = smootherARorder(orderHold, orderHold, orderHold, 0, timeHold, x*ba.db2linear(limGain*FBstrength)) ;
     holdEnvPrevDiv(prevDiv,x) = smootherARorder(orderHold, orderHold, orderHold, 0,
                                                 it.interpolate_linear(prevDiv,timeHold,timeHold*slowGroup(hslider("mult hold", 0.1, 0, 1, 0.001)))
                                                 , x*ba.db2linear(limGain*FBstrength));
     attRelEnv(x) = smootherARorder(maxOrder, orderRel, orderAtt, timeRel, timeAtt, x);
     attRelEnvPrevDiv(prevDiv,x) =
       smootherARorder(maxOrder, orderRel, orderAtt, it.interpolate_linear(prevDiv,timeRel,timeRel*slowGroup(hslider("mult rel", 0.1, 0, 1, 0.001))),  timeAtt, x)
       <:(
       (
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


     relEnv(x) = smootherARorder(maxOrder,1, orderRel, 0, timeRel, x);
   };
   lin2log(minX, maxX,x) =
     it.remap(
       minX, maxX, ba.hz2midikey(minX), ba.hz2midikey(maxX),x
     )
     : ba.midikey2hz;

   log2lin(minX, maxX,x) =
     ba.hz2midikey(x)
     :it.remap(
       ba.hz2midikey(minX), ba.hz2midikey(maxX)
       ,minX, maxX
     );


   ///////////////////////////////////////////////////////////////////////////////
   //                               groups                                     //
   ///////////////////////////////////////////////////////////////////////////////
   // mainGroup(x) = vgroup("main", x);
   slowGroup(x) = vgroup("slow", x);
   ///////////////////////////////////////////////////////////////////////////////
   //                               GUI                                     //
   ///////////////////////////////////////////////////////////////////////////////

   symmetry = symmetryGUI(guiType);
   symmetryGUI(0) =  hslider("[3]symmetry", 1, 0, 1, 0.001):si.smoo;
   symmetryGUI(1) = symmetryGUI(0);
   symmetryGUI(2) = hslider("[3]symmetry[style:knob][stratus:3]", 10, 0, 10, 0.1)*0.1:si.smoo;
   symmetryGUI(3) = hslider("[3]symmetry[style:knob][stratus:3]", 1, 0, 1, 0.01):si.smoo;

   meterGUI(0) = hbargraph("GR", -24, 0);
   meterGUI(1) = meterGUI(0);
   meterGUI(x) = _;
   maxOrder = 2;
   // orderHold = 16;
   // hslider("order hold", maxOrder, 1, maxOrder, 1);
   orderAtt =
     2;
   // hslider("order att", 4, 1, maxOrder, 1);
   orderAttSlow =
     // 2;
     hslider("order att slow", 1, 1, maxOrder, 1);
   orderRelSlow =
     // 2;
     hslider("order rel slow", 1, 1, maxOrder, 1);

   orderRel = 2;
   // hslider("order rel", 2, 1, maxOrder, 1);
   orderRelLim =
     2;
   // hslider("order rel lim", 2, 1, maxOrder, 1);
   timeHold = timeHoldGUI(guiType);
   timeHoldGUI(0) = hslider("[11]hold time[scale:log]", 0.272, 0.013, 0.5, 0.001);
   timeHoldGUI(x) = timeRel;
   timeHoldLim = timeHoldLimGUI(guiType);
   // timeRel*0.333;
   timeHoldLimGUI(0) = hslider("hold time lim[scale:log]", 0.013, 0.001, 0.2, 0.001);
   timeHoldLimGUI(x) = 0.013;
   timeAtt = timeAttGUI(guiType);
   timeAttGUI(0) = hslider("[01]attack[scale:log]", 0.001, 0.001, 0.05, 0.001)-0.001;
   timeAttGUI(x) = 0;
   timeAttSlow = hslider("att time slow[scale:log]", 0.2, 0.001, 1, 0.001)-0.001;
   timeRelSlow = timeRelSlowGUI(guiType);
   // hslider("rel time slow[scale:log]", 0.2, 0.001, 0.5, 0.001)-0.001;
   timeAttHold = hslider("att hold time[scale:log]", 0.001, 0.001, 0.1, 0.001)-0.001;
   timeRel = timeRelGUI(guiType);
   timeRelGUI(2) = hslider("[1]release[style:knob][stratus:1]",
                           it.remap(0.013,0.5,0,10, log2lin(0.013,0.5,0.272))
                           , 0, 10 , 0.001)
                   :it.remap(0,10,0.013,0.5)
                   :lin2log(0.013,0.5) ;
   timeRelGUI(3) = timeRelGUI(2);
   timeRelGUI(x) = hslider("[1]release[scale:log]", 0.272, 0.013, 0.5, 0.001);
   // hslider("[%index] att CF [style:knob]",
   // log2lin(minX, maxX, default),
   // minX,
   // maxX,
   // log2lin(minX, maxX, stepsize)
   // ) :lin2log(minX, maxX)


   timeRelLim = timeRelLimGUI(guiType);
   timeRelLimGUI(0) = hslider("rel time lim[scale:log]", 0.024, 0.001, 0.1, 0.001);
   timeRelLimGUI(x) = 0.024;

   inputGain = inputGainGUI(guiType);
   inputGainGUI(0) = hslider("[01]input gain[unit:dB]", 30, 0, 30, 0.1)
                     // :ba.db2linear
                     :si.smoo
   ;
   inputGainGUI(1) = inputGainGUI(0);
   inputGainGUI(2) = hslider("[0]input gain[style:knob][stratus:0]", 10, 0, 10, 0.001):it.remap(0,10,0,30) :si.smoo;
   inputGainGUI(3) = hslider("[0]input gain[style:knob][stratus:0]", 30, 0, 30, 0.1):si.smoo;
   outputGain = outputGainGUI(guiType);
   outputGainGUI(0) = hslider("[2]output gain[unit:dB]", 0, -30, 0, 0.1)
                      // :ba.db2linear
                      :si.smoo
   ;
   outputGainGUI(1) = outputGainGUI(0);
   outputGainGUI(2) = hslider("[2]output gain[style:knob][stratus:2]", 10, 0, 10, 0.001):it.remap(0,10,-30,0) :si.smoo ;
   outputGainGUI(3) = hslider("[2]output gain[style:knob][stratus:2]", 0, -30, 0, 0.1):si.smoo ;
   mix = mixGUI(guiType);
   // mixGUI(0) = hslider("[2]mix", 1, 0, 1, 0.001):si.smoo;
   // mixGUI(1) = mixGUI(0);
   // mixGUI(2) = hslider("[2]mix[style:knob][stratus:2]", 10, 0, 10, 0.1):it.remap(0,10,0,1):si.smoo;
   // mixGUI(3) = hslider("[2]mix[style:knob][stratus:2]", 1, 0, 1, 0.01):si.smoo;
   hpFreq = hslider("high pass freq", 20, 2, 40, 1);
   strength = strengthGUI(guiType);
   strengthGUI(0) = hslider("[02]strength[unit:%]", 100, 0, 100, 1) * 0.01;
   strengthGUI(x) = 1;
   FBstrength = FBstrengthGUI(guiType);
   FBstrengthGUI(0) = hslider("[02]FB strength[unit:%]", 50, 0, 100, 1) * 0.01;
   FBstrengthGUI(x) = 0.5;
   // TODO: make thres relative to limThresh?
   // we need some headroom, will distort at higher threshold
   // TODO: does it distort because of the symetry
   thresh = threshGUI(guiType);
   threshGUI(0) = hslider("[11]threshold[unit:dB]",-1.5,-30,0,0.1);
   threshGUI(x) = -1.5;
   limThresh = limThreshGUI(guiType);
   limThreshGUI(0) = hslider("[14]lim threshold[unit:dB]",0,-10,0,0.1);
   limThreshGUI(x) = 0;
   knee = kneeGUI(guiType);
   kneeGUI(0) = hslider("[17]knee[unit:dB]",0,0,30,0.1);
   kneeGUI(x) = 0;

   orderDCatt =
     4;
   // hslider("order att DC", 4, 1, maxOrder, 1);
   orderDCrel =
     4;
   // hslider("order rel DC", 4, 1, maxOrder, 1);
   timeDCatt = timeDCattGUI(guiType);
   timeDCattGUI(0) =
     (hslider("att time DC[scale:log]", 0.021, 0.001, 0.10, 0.001)-0.001)*0.1;
   timeDCattGUI(x) = 0.002;
   symmetryRel =
     symmetryRelGUI(guiType);
   symmetryRelGUI(0) = hslider("[31]rel time DC[scale:log]", 0.272, 0.013, 0.5, 0.001);
   symmetryRelGUI(x) = timeRel;

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
     limGain
   , (offset(x) * symmetry)
   , x
   with {
     positiveEnv(x) =
       // smootherARorder(maxOrder, orderDCrel, orderDCatt, symmetryRel, timeDCatt, x*(x<0));
       smootherARorder(maxOrder, orderDCrel, 1, timeRelHold, 0, x*(x<0))
     ;
     // :smootherARorder(maxOrder, 1, orderDCatt, 0, timeDCatt) ;
     negativeEnv(x) =
       // smootherARorder(maxOrder,  orderDCatt, orderDCrel, timeDCatt, symmetryRel, x*(x>0));
       smootherARorder(maxOrder,  1, orderDCrel, 0, timeRelHold, x*(x>0))
     ;
     // : smootherARorder(maxOrder,orderDCatt, 1, timeDCatt, 0);
     offset(x) =
       PosNeg(x):+ *.5
       // ((positiveEnv(x)+negativeEnv(x))*.5)

       *
       // select2(1-checkbox("lim DC"),1,
       ba.db2linear(limGain)
       // )
       // TODO: needed??
       : smootherARorder(maxOrder,orderDCatt, orderDCatt, timeDCatt , timeDCatt);
   };

   PosNeg(x) =
     positiveEnv(x)
   , negativeEnv(x)
   with {
     positiveEnv(x) =
       // smootherARorder(maxOrder, orderDCrel, orderDCatt, symmetryRel, timeDCatt, x*(x<0));
       smootherARorder(maxOrder, orderDCrel, 1, 0, timeHold, x*(x>0))
     ;
     // :smootherARorder(maxOrder, 1, orderDCatt, 0, timeDCatt) ;
     negativeEnv(x) =
       // smootherARorder(maxOrder,  orderDCatt, orderDCrel, timeDCatt, symmetryRel, x*(x>0));
       smootherARorder(maxOrder,  1, orderDCrel, timeHold, 0, x*(x<0))
     ;
   };
