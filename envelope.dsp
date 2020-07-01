import("stdfaust.lib");
declare author "Bart Brouns";
declare license "GPLv3";

xVal = hslider("xVal", 0, 0, maxSamples, 1)/defaultSR*ma.SR;
yVal = hslider("yVal", 0, -1, 1, 0.001);
slope = hslider("slope", 0, -1, 1, 0.001);
goBtn = button("resetBtn"):ba.impulsify;

maxSeconds = 5;
maxSamples = maxSeconds*defaultSR;
defaultSR = 48000;

process =
  envelope;

// ramp from 0 to 1 in n samples.
// when reset == 1, go back to 0.
ramp(n,reset) = select2(reset,_+(1/n):min(1),0)~_;


xVal_target(reset)  = rwtable(3, 0.0,nextValIndex(reset),xVal,currentValIndex(reset));
yVal_target(reset)  = rwtable(3, 0.0,nextValIndex(reset),yVal,currentValIndex(reset));

currentValIndex(reset) = reset :ba.toggle: xor(startPulse'');
// currentValIndex(reset) = (reset|startPulse) :ba.toggle;
nextValIndex(reset) = currentValIndex(reset)*-1+1;

startPulse = 1-1';

xfade(a,b,x) = a*(1-x)+b*x;

envelope = FB~_ with {
  FB(prev) = ((xfade(ba.sAndH(reset),yVal_target(reset),position)+(yVal*startPulse))~(_))
            ,(reset':ba.impulsify:hbargraph("[1]endpoint", 0, 1))
  with {
  reset = ( (prev==yVal_target(_)) :xor(goBtn): hbargraph("reset", 0, 1) )~(_<:(_,_));
  position = ramp(xVal_target(reset),reset):hbargraph("[0]position", 0, 1);
  // the commented out stuff is WIP for changing the curve of the ramp
  // position = ramp(xVal_target(reset),reset):hbargraph("pos", 0, 1):pow(power):max(0):min(1);
  };
  // power = select2(slope<0,slope*px+1,1+slope*mx):hbargraph("power", 0, 10);
  // px = hslider("px", 1, 1, 9, 1);
  // mx = hslider("mx", 0.5, 0, 1, 0.001);
};

//********************************************************************************************
//**** bezier
//********************************************************************************************
// see for a different approach: https://www.cubic.org/docs/hermite.htm
// http://news.povray.org/povray.binaries.tutorials/attachment/%3CXns91B880592482seed7@povray.org%3E/Splines.bas.txt
// process = cubic_bezier(0,0, p1x, p1y, p2x, p2y, 1,1, os.lf_sawpos(0.5));
// https://www.musicdsp.org/en/latest/Other/93-hermite-interpollation.html

p1x = hslider("p1x", 0, -2, 2, 0.1);
p1y = hslider("p1y", 0, -2, 2, 0.1);
p2x = hslider("p2x", 0, -2, 2, 0.1);
p2y = hslider("p2y", 0, -2, 2, 0.1);

cubic_bezier(p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y, t) = pFinalX, pFinalY
with {
  pFinalX = pow(1 - t, 3) * p0x +
pow(1 - t, 2) * 3 * t * p1x +
(1 - t) * 3 * t * t * p2x +
t * t * t * p3x;
  pFinalY = pow(1 - t, 3) * p0y +
pow(1 - t, 2) * 3 * t * p1y +
(1 - t) * 3 * t * t * p2y +
t * t * t * p3y;
};


// cubicBezier: function(p0, p1, p2, p3, t, pFinal) {
// pFinal = pFinal || {};
// pFinal.x = Math.pow(1 - t, 3) * p0.x +
// Math.pow(1 - t, 2) * 3 * t * p1.x +
// (1 - t) * 3 * t * t * p2.x +
// t * t * t * p3.x;
// pFinal.y = Math.pow(1 - t, 3) * p0.y +
// Math.pow(1 - t, 2) * 3 * t * p1.y +
// (1 - t) * 3 * t * t * p2.y +
// t * t * t * p3.y;
// return pFinal;
// }

// TODO: PR:
// process = lf_sawpos_phase_trig(440,phase,reset);
// phase = hslider("phase", 0, 0, 1, 0.001);

// lf_sawpos_trig(freq,phase,trig) = ma.frac * dontReset ~ +(freq/ma.SR):_+phase:ma.frac
// with {
// dontReset  = trig:ba.impulsify*-1+1;
// };
