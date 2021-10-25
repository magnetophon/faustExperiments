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
// lookahead = pow(2,LA);
lookahead = 1<<LA;
LA = 9;
// table of indexes when to start a new ramp
indexes(N,lookahead, input) = par(i, N, rwtable(size+1, init, windex, input, rindex)) with {
  windex = (_%lookahead)~(_+trig);
};

///////////////////////////////////////////////////////////////////////////////
//                               implementation                              //
///////////////////////////////////////////////////////////////////////////////

process =
  // getIndexAndDirection;
  // find_Nmin(LA);
  GR@(lookahead-1),
  (GR: Limiter)
;
// (GR:SimpleLimiter);

find_Nmin(N) = seq(i,N-1, (Ncomparator,si.bus(2*(N-i-1)))) : Ncomparator;

Ncomparator(n,x,m,y) = select2((x<=y),m,n), select2((x<=y),y,x); // compare integer-labeled signals

Limiter =
  // getNewOutputs
  (
    ro.crossn1(3)
    :
    (( sequentialMinimumParOut(LA), ((split,split,split):ro.interleave(2,3)))
     : (si.bus(LA),split,si.bus(6))
     : (si.bus(LA+1),ro.crossNM(1,6))
     : getIndexAndDirection,si.bus(4):getNewOutputs
    ))~si.bus(3):(_,_/lookahead,_*lookahead)
with {
  split = (_<:(_,_));
  getDirection =
    (
      si.bus(LA+1)
    , (_ <: si.bus(LA+1))
    )
    :
    (
      ro.interleave(LA+1,2)
      : par(i, LA+1,(- / (1<<i)) )
      : minOfN(LA+1)
    );
  getNewOutputs(index, direction, oldGain, oldIndex, oldDirection,lowestGain) =
    (getNewIndex<: (newAttack:newRelease),_,(_>0)*direction)
  with {
    getNewIndex =
      select2((index>oldIndex)// & (direction!=oldDirection)
             ,oldIndex-1
             ,index
             );
    newAttack =
      // !:(button("reset")==0)*direction+oldGain;
      indexToGainMulti*direction+oldGain;
    indexToGainMulti(index) =
      // (index/lookahead)*-1+1;
      (index>0);
    newRelease(newAttack) =
      select2(newAttack<lowestGain
             , newAttack
             , (lowestGain-oldGain)/hslider("release", lookahead, 1, lookahead*4, 1)+oldGain
             );

  };

  attack                  = hslider("[2]attack shape[tooltip: 0 gives a linear attack (slow), 1 a strongly exponential one (fast)]", 1 , 0, 1 , 0.001);
  attackShaper(x)= ma.tanh(x:pow(attack:attackScale)*(attack*5+.1))/ma.tanh(attack*5+.1);
  attackScale(x) = (x+1):pow(7); //from 0-1 to 1-128, just to make the knob fit the aural experience better
  // (oldGain, index, direction );
};

getIndexAndDirection =
  (
    si.bus(LA)
  , prep
  ):
  (ro.crossNM(LA+2,2)
   :
   (
     si.bus(LA+3)
   , (_ <: si.bus(LA+1))
   )
  )
  :
  (_,_),
  (
    ro.interleave(LA+1,2)
    : par(i, LA+1, (1<<i),(- / (1<<i)) )
  )
  :
  find_Nmin(LA+1)
with {
  prep(lowestGain,prevGain,prevIndex,prevDirection) =
    (lowestGain,prevGain,prevIndex,select2(prevGain<=lowestGain, prevDirection, ma.INFINITY));
};

minGRmeter = min(1):max(-1):hbargraph("minGRmeter", -1, 1);

SimpleLimiter =
  (
    ro.cross(2)
    :( sequentialMinimum(LA), (_<:(_,_)))
    : (
      ((-:minGRmeter) / (1<<(LA-1)))<:(_,_)
    ) ,(_:+)
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



selectFromN(N, sel) = par(i, N, _*(i==sel)):>_;



GR = no.lfnoise0(lookahead *t * (no.lfnoise0(lookahead/2):max(0.1) )):pow(3)*(1-noiseLVL) +(no.lfnoise(rate):pow(3) *noiseLVL):min(0);//(no.noise:min(0)):ba.sAndH(t)
                                                                                                                               t= hslider("time", 0.1, 0, 1, 0.001);
                                                                                                                               noiseLVL = hslider("noise", 0, 0, 1, 0.01);
                                                                                                                               rate = hslider("rate", 20, 10, 20000, 10);
