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
LA = 13;
LAconvex = 128;
SEQnr = 32;
// table of indexes when to start a new ramp
indexes(N,lookahead, input) = par(i, N, rwtable(size+1, init, windex, input, rindex)) with {
  windex = (_%lookahead)~(_+trig);
};

///////////////////////////////////////////////////////////////////////////////
//                               implementation                              //
///////////////////////////////////////////////////////////////////////////////

process =
  // getIndexAndDirection(0,LA);
  // find_Nmin(LA);
  // (
  // ((GR:sequentialLinMinimumParOut(LAconvex))
  // , (_<:si.bus(LAconvex+2))
  // )
  // : (ro.interleave(LAconvex+1,2),_)
  // : (par(i, LAconvex+1, -/(i+1)),_)
  // : (maxOfN(LAconvex+1),_)
  // :+)~_
  // ,GR@(LAconvex)

  // hold(10)
  // (GR:LimiterConvex(LA)),
  ((GR:hold(tCon):Limiter(0,LA-1)@(lookahead-(1<<(LA-1)))),
   (GR:hold(bCon):Limiter(0,LA-2)@(lookahead-(1<<(LA-2))))
  ):max
    // ,(GR:hold(tCon):Limiter(0,LA-1)@(lookahead-(1<<(LA-1)))),
    // (GR:hold(bCon):Limiter(0,LA-2)@(lookahead-(1<<(LA-2))))
, GR@(maxHold+lookahead-1)
, (GR:LimiterConvex(LA))

  // (GR:ba.slidingMin(LAconvex,LAconvex))
  // , (GR:ba.slidingMin(64,LAconvex)@64)
  // , (GR:ba.slidingMin(32,LAconvex)@96)
  // , GR@LAconvex
  //
  // GR@(3*lookahead-3)
  // ,((GR: Limiter@(2*lookahead-2)))
  // , (GR: Limiter: Limiter@(lookahead-1))
  // , (GR: Limiter: Limiter: Limiter)
  //
  // GR@(SEQnr*lookahead-offset)
  // , (GR<:seq(i, SEQnr, select2((i+1)<=SEQamount,_@(lookahead-1), Limiter)<:(_,_) ))
;
SEQamount = hslider("seq amount", 1, 1, SEQnr, 1);
offset = hslider("offset", 0, 0, 2*SEQnr, 1);
maxHold = 1<<(LA-1);
holdTime = hslider("holdTime", 0, 0, maxHold, 1);
// (GR:SimpleLimiter);

find_Nmin(N) = seq(i,N-1, (Ncomparator,si.bus(2*(N-i-1)))) : Ncomparator;

Ncomparator(n,x,m,y) = select2((x<=y),m,n), select2((x<=y),y,x); // compare integer-labeled signals

LimiterConvex(LA) =
  (
    paramArray(bCon,mCon,band,tCon,LA)
    // par(i, LA, 10)
  , (_<:si.bus(LA))
  ):ro.interleave(LA,2)
  :par(i, LA,
       (hold:Limiter(0,i+1)@(lookahead-(1<<(i+1))))
      )
  :maxOfN(LA)
;
bCon = hslider("bottom", 0, 0, maxHold, 1);
mCon = hslider("mid", 0, 0, maxHold, 1);
tCon = hslider("top", 0, 0, maxHold, 1);
LimiterDuo(LA) =
  _<:max(
    Limiter(0,LA)
  , (hold(holdTime):Limiter(1,LA-1))
    // , (ba.slidingMin(holdTime,maxHold):Limiter(1,LA-1)@(maxHold-holdTime))
  );
hold(holdTime) = ba.slidingMin(HT,maxHold)@(maxHold-HT) with {
  HT = holdTime:int:max(0):min(maxHold);
};


LimiterDuoOLD(LA) =
  // getNewOutputs
  split
  :(
  (ro.crossn1(6),_
                 // ,si.bus(4)
  )
  :(si.bus(4),ro.crossn1(3))
  :
  (
    (( sequentialMinimumParOut(LA), ((split,split,split):ro.interleave(2,3)))
     : (si.bus(LA),split,si.bus(6))
     : (si.bus(LA+1),ro.crossNM(1,6))
     : getIndexAndDirection(0,LA),si.bus(4):getNewOutputs
    )
  ,
    (( sequentialMinimumParOut(LA), ((split,split,split):ro.interleave(2,3)))
     : (si.bus(LA),split,si.bus(6))
     : (si.bus(LA+1),ro.crossNM(1,6))
     : getIndexAndDirection(1,LA),si.bus(4):getNewOutputs
    ))
)~si.bus(6)
  :(_,_/lookahead,!,_,_/lookahead,!)
  :(_,ro.cross(2),_)
   // :(max,_,_)
  :(max,_,_)
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
    // newRelease(newAttack) =
    // select2(oldGain<=lowestGain
    // , newAttack:max(lowestGain)
    // , lowestGain
    // );
    newRelease(newAttack) =
      select2(oldGain<=lowestGain
             , newAttack:max(lowestGain)
             , (lowestGain-oldGain)/hslider("release", lookahead/4, 1, lookahead*4, 1)+oldGain
             );

  };

  attack                  = hslider("[2]attack shape[tooltip: 0 gives a linear attack (slow), 1 a strongly exponential one (fast)]", 1 , 0, 1 , 0.001);
  attackShaper(x)= ma.tanh(x:pow(attack:attackScale)*(attack*5+.1))/ma.tanh(attack*5+.1);
  attackScale(x) = (x+1):pow(7); //from 0-1 to 1-128, just to make the knob fit the aural experience better
  // (oldGain, index, direction );
};

Limiter(Shaped,LA) =
  // getNewOutputs
  (
    (ro.crossn1(3)
     // ,si.bus(4)
    )
    :
    (( sequentialMinimumParOut(LA), ((split,split,split):ro.interleave(2,3)))
     : (si.bus(LA),split,si.bus(6))
     : (si.bus(LA+1),ro.crossNM(1,6))
     : getIndexAndDirection(Shaped,LA),si.bus(4):getNewOutputs
    )
  )~si.bus(3)
    :(_,!,!)
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
      // newAttack;
      select2(oldGain<=lowestGain
             , newAttack:max(lowestGain)
             , (lowestGain-oldGain)/hslider("release", lookahead/4, 1, lookahead*4, 1)+oldGain
             );

  };

  attack                  = hslider("[2]attack shape[tooltip: 0 gives a linear attack (slow), 1 a strongly exponential one (fast)]", 1 , 0, 1 , 0.001);
  attackShaper(x)= ma.tanh(x:pow(attack:attackScale)*(attack*5+.1))/ma.tanh(attack*5+.1);
  attackScale(x) = (x+1):pow(7); //from 0-1 to 1-128, just to make the knob fit the aural experience better
  // (oldGain, index, direction );
};

getIndexAndDirection(0,LA) =
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
    (lowestGain,prevGain,prevIndex,select2(prevGain<lowestGain, prevDirection, ma.INFINITY));
  // (lowestGain,prevGain,prevIndex,ma.INFINITY);
};

getIndexAndDirection(1,LA) =
  (
    si.bus(LA)
  , prep
  ):
  (ro.crossNM(LA+2,2)
   :
   (
     si.bus(LA+3)
   , (_ <: si.bus(LA+1))
   , paramArray(bottom,mid,band,top,LA+1)
   )
  )
  :
  (_,_),
  (
    ro.interleave(LA+1,3)
    : par(i, LA+1, (1<<i),(((- / (1<<i)),_):*) )
  )
  :
  find_Nmin(LA+1)
with {
  prep(lowestGain,prevGain,prevIndex,prevDirection) =
    (lowestGain,prevGain,prevIndex,select2(prevGain<lowestGain, prevDirection, ma.INFINITY));
  // (lowestGain,prevGain,prevIndex,ma.INFINITY);
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
maxOfN(N) = opOfN(N,max);

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

sequentialLinOperatorParOut(N,op) =
  // operator(2)
  seq(i, N, operator(i))
  : par(i, N+1, _@( N - i ))
with {
  operator(i) =
    myBus(i)
  ,
    (_<:
     _ , op(_,_' )
    )
  ;
};

sequentialLinMinimumParOut(N) = sequentialLinOperatorParOut(N,min);

// LazyLeveler(N);

LazyLeveler(N) = (fillTable(N):useTable(N))~feedback(N);

fillTable(N) = par(i, N*2, _):> par(i, N, _);
useTable(N) = par(i, N, _);
feedback(N) = par(i, N, _);

myBus(0) = 0:!;
           myBus(i) = si.bus(i);



selectFromN(N, sel) = par(i, N, _*(i==sel)):>_;

paramArray(bottom,mid,band,top,LA) =
  par(i, LA, select2(band<=i+1,midToBottomVal(i),midToTopVal(i)))
with {
  midToBottomVal(i) = (midToBottom(i)*bottom) + (((midToBottom(i)*-1)+1)*mid);
  midToBottom(i) = (band-(i+1))/(band-1);

  midToTopVal(i) = (midToTop(i)*top) + (((midToTop(i)*-1)+1)*mid);
  midToTop(i) = (i+1-band)/((LA+1)-band);
};

top = hslider("[0]top", 0.01, 0, 1, 0.01);
mid = hslider("[1]mid", 0.2, 0, 1, 0.01);
band = hslider("[2]band", (LA-(LA/8)):int, 0, LA+1, 1);
bottom =  hslider("[3]bottom", 4.2, 1, 20, 0.1);


GR = no.lfnoise0(lookahead *t * (no.lfnoise0(lookahead/2):max(0.1) )):pow(3)*(1-noiseLVL) +(no.lfnoise(rate):pow(3) *noiseLVL):min(0);//(no.noise:min(0)):ba.sAndH(t)
                                                                                                                               t= hslider("time", 10, 0.1, 10, 0.1);
                                                                                                                               noiseLVL = hslider("noise level", 0, 0, 1, 0.01);
                                                                                                                               rate = hslider("rate [scale:log]", 4200, 10, 10000, 1);
