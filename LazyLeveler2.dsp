declare name "LazyLeveler";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

process =
  // attackArray(LA)
  // : par(i, LA, hbargraph("lev%i",0,1.4))
  LazyLeveler(LA,testSig(LA+3))~_
                                // :par(i, 3, _@200000)
                                // :par(i, 5, _*.5)
with {
  LA = 14;
};


LazyLeveler(LA,x,prevGain) =
  // linearAttack(LA,x,prevGain)
  shapedAttack(LA,x,prevGain)
;


shapedAttack(LA,x,prevGain) =
  (x:sequentialMinimumParOut(LA)
     // :(par(i, LA, !),_)
   : (!,si.bus(LA))
   : par(i, LA, getDirection(i+1,prevGain))
   : ( ro.interleave(2,LA) , attackArray(LA)
     )
   : (si.bus(LA),ro.crossnn(LA))
   : (ro.interleave(LA,2),si.bus(LA))
   : (par(i, LA, *), si.bus(LA))
   : ro.interleave(LA,2)
   : find_Nmin(LA)
     // :> (_,_)
     // : ro.interleave(2,LA)
     // : par(i, 2, minOfN(LA))
   : (getGain(LA,x,prevGain))
  )
, (x@lookahead(LA))
with {
  // getGain(LA,x,prevGain,index,direction,minGain)=
  getGain(LA,x,prevGain,direction,minGain)=
    direction+ prevGain:min(0):min(x@lookahead(LA)):max(minGain)
  , minGain
    // , (index/lookahead(LA))
  ;
};

linearAttack(LA,x,prevGain) =
  (x:sequentialMinimumParOut(LA)
     // :(par(i, LA, !),_)
   : (!,si.bus(LA))
   : par(i, LA, getDirection(i+1,prevGain))
   : find_Nmin(LA)
     // : ro.interleave(2,LA)
     // : par(i, 2, minOfN(LA))
   : (getGain(LA,x,prevGain))
  )
, (x@lookahead(LA))
with {
  // getGain(LA,x,prevGain,index,direction,minGain)=
  getGain(LA,x,prevGain,direction,minGain)=
    direction+ prevGain:min(0):min(x@lookahead(LA)):max(minGain)
  , minGain
    // , (index/lookahead(LA))
  ;
};



getDirection(LA,prevGain,minGain) =
  (getIndexAndDirection~(_,_))
  : (!,_,minGain)
    // : (_,_,minGain)
with {
  getIndexAndDirection(prevIndex,prevDirection) =
    index,direction
  with {
  index =
    select2(
      ((minGain:ba.sAndH(trig)-prevGain)/(lookahead(LA)))<0
      // proposedDirection<0
    , 0
    , select2(trig
             , ((prevIndex-1)
                :max(0)
               )
             , lookahead(LA)));
  direction =
    select2(trig
           ,  (minGain:ba.sAndH(trig)-prevGain) / ((prevIndex):max(1))
           , (minGain-prevGain)/lookahead(LA)
           )
  ;
  trig = (proposedDirection<=(prevGain-prevGain'));
  proposedDirection = (dif/(lookahead(LA)));
  dif = minGain-prevGain;
};
};


sequentialMinimumParOut(N) = sequentialOperatorParOut(N,min);

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

paramArray(bottom,mid,band,top,LA) =
  par(i, LA, select2(band<=i+1,midToBottomVal(i),midToTopVal(i)))
with {
  midToBottomVal(i) = (midToBottom(i)*bottom) + (((midToBottom(i)*-1)+1)*mid);
  midToBottom(i) = (band-(i+1))/(band-1);

  midToTopVal(i) = (midToTop(i)*top) + (((midToTop(i)*-1)+1)*mid);
  midToTop(i) = (i+1-band)/((LA+1)-band);
};



opOfN(N,op) = seq(i, N-1, op,myBus(N-i-2));
minOfN(N) = opOfN(N,min);
maxOfN(N) = opOfN(N,max);

find_Nmin(N) = seq(i,N-2, (Ncomparator,si.bus(2*(N-i-2)))) : Ncomparator;
Ncomparator(x,n,y,m) = select2((x<=y),y,x), select2((x<=y),m,n); // compare integer-labeled signals

lookahead(LA) = (1<<LA)-1;

myBus(0) =
  0:!
;
myBus(i) = si.bus(i);

attackArray(LA) =
  paramArray(1.4,0,attack2band(LA+1) ,0,LA+1)
  : (si.bus(LA),!)
;

attack = hslider("[1]attack", 1, 0, 1, 0.01);
// attack2band(LA) = (attack*LA);
attack2band(LA) = 2+(attack*(LA-2)):max(2);
// attack2band(LA) = hslider("band", 0, 0, LA, 0.1);


testSig(LA) =
  // checkbox("tst")*-1;
  // button:ba.impulsify*-1;
  no.lfnoise0(lookahead(LA) *t * (no.lfnoise0(lookahead(LA)/2):max(0.1) )):pow(3)*(1-noiseLVL) +(no.lfnoise(rate):pow(3) *noiseLVL):min(0);
t= hslider("[7]time", 2.18, 0.01, 4, 0.01):pow(2);
noiseLVL = hslider("[8]noise level", 0, 0, 1, 0.01);
rate = hslider("[9]rate [scale:log]", 420, 10, 10000, 1);
declare name
