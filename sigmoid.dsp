declare name "sigmoid";
declare version "0.1";
declare author "Bart Brouns";
declare license "AGPL-3";

declare credit "GT4 2014-2015 Modification of a sine wave using a 3 stage NTSF chain. Version 1. Dino Dini. 29/04/2015";

import("stdfaust.lib");

fNts(x,k)= ( (x-x*k)/(k-abs(x)*2*k+1) );
fC(x) = (x*0.5 + 0.5);
fD(x) = 2*x-1;
fNts3(x,k1,k2,k3) = fD(fNts(fC(fNts(fD(fNts(fC(x),k1)),k2)),k3));
sinFnts3(x,k1,k2,k3) = (fNts3(sin((x-0.5)*ma.PI),k1,k2,k3)+1);

preShaper(x,k1,k2,k3) = (fNts3(x*2-1,k1,k2,k3)+1)*0.5;
sigmoid(x,k1,k2,k3) = sinFnts3(x,k1,k2,k3)*0.5;

saw = os.lf_sawpos(20);

k1 = hslider("k1", 0, -1+ma.EPSILON, 1-ma.EPSILON, 0.01);
k2 = hslider("k2", 0, -1+ma.EPSILON, 1-ma.EPSILON, 0.01);
k3 = hslider("k3", 0, -1+ma.EPSILON, 1-ma.EPSILON, 0.01);
// k3 = k1*-1;



// process =
// saw
// , preShaper(saw,k1,k2,k3)
// , sigmoid(saw,k1,k2,k3);
