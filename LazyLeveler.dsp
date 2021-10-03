declare name "LazyLeveler";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

///////////////////////////////////////////////////////////////////////////////
//                                 variables                                 //
///////////////////////////////////////////////////////////////////////////////

// nr of channels
N = 2;
// nr of lookahead samples
lookahead = pow(2,LA);
LA = 8;
// table of indexes when to start a new ramp
indexes(N,lookahead, input) = par(i, N, rwtable(size+1, init, windex, input, rindex)) with {
  windex = (_%lookahead)~(_+trig);
};

///////////////////////////////////////////////////////////////////////////////
//                               implementation                              //
///////////////////////////////////////////////////////////////////////////////

process =
  GR@(lookahead-1),
  (GR:SimpleLimiter);
// (GR:Limiter);

Limiter =
  (
    ro.cross(2)
    :( sequentialMinimumParOut(LA), (_<:(_,_)))
    : getDirection,_:+
  )~_ with {
  getDirection =
    (
      si.bus(LA+1)
    , (_ <: si.bus(LA+1))
    )
    :
    (
      ro.interleave(LA+1,2)
      : par(i, LA+1,- / (1<<i) )
        // : par(i, LA+1,- / ((1<<i)*hslider("offset", 1, 0, 2, 0.001):max(1)) )
      : minOfN(LA+1)
    );
};

minGRmeter = min(1):max(-1):hbargraph("minGRmeter", -1, 1);

SimpleLimiter =
  (
    ro.cross(2)
    :( sequentialMinimum(LA), (_<:(_,_)))
    : (
      (-:minGRmeter) / (1<<(LA-1))
    ),_:+
  )~_;

opOfN(N,op) = seq(i, N-1, op,myBus(N-i-2));
minOfN(N) = opOfN(N,min);

// sequentialMinimum(N) = seq(i, N, minimum(i)) with {
// minimum(i) = _<:min(_, _@(1<<i) );
// };

sequentialOperator(N,op) = seq(i, N, operator(i)) with {
  operator(i) = _<:op(_, _@(1<<i) );
};

sequentialMinimum(N) = sequentialOperator(N,min);

sequentialOperatorParOut(N,op) =
  // operator(2)
  seq(i, N, operator(i))
  : par(i, N+1, _@( 1<<N - 1<<i ))
with {
  operator(i) =
    myBus(i)
  ,
    (_<:
     _ , op(_,_@(1<<i) )
    )
  ;
};

sequentialMinimumParOut(N) = sequentialOperatorParOut(N,min);

// LazyLeveler(N);

LazyLeveler(N) = (fillTable(N):useTable(N))~feedback(N);

fillTable(N) = par(i, N*2, _):> par(i, N, _);
useTable(N) = par(i, N, _);
feedback(N) = par(i, N, _);

myBus(0) = 0:!;
           myBus(i) = si.bus(i);
GR = no.lfnoise0(lookahead *t * (no.lfnoise0(lookahead/2):max(0.1) )):pow(3)*(1-noiseLVL) +(no.lfnoise(rate):pow(3) *noiseLVL):min(0);//(no.noise:min(0)):ba.sAndH(t)
                                                                                                                               t= hslider("time", 0.1, 0, 1, 0.001);
                                                                                                                               noiseLVL = hslider("noise", 0, 0, 1, 0.01);
                                                                                                                               rate = hslider("rate", 20, 10, 20000, 10);
