declare name "LazyLeveler";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

process =
  // attackArray(LA)
  // : par(i, LA, hbargraph("lev%i",0,1.4))

  LazyLeveler(LA,testSig(LA-5))

  // blokjes
  // sequentialBlockMinimumParOut(nrBlocks,lookahead(LA))
  // slidingReduce(min,lookahead(LA),lookahead(LA),ma.INFINITY)

  // testSig(LA):
  // (convexAttack(nrBlocks,LA,Lookah))

  // ,_
  // @Lookah
  // )
  // :par(i, 2, _@200000)
  // :par(i, 5, _*.5)
with {
  Lookah = hslider("Lookah", 0, 0, lookahead(LA), 1);
  holdTime= Lookah;
  LA = 13;
  // nrBlocks = 3;
  // blokjes(x,n) =   sequentialBlockOperatorParOut(n,min,ma.INFINITY,lookahead(LA),x);
};

//TODO: each stage caries it's GR and its properly delayed audio

LazyLeveler(LA,x) =
  // (linearAttack(LA,x)~_)
  // x
  // : (linAtt(LA-4)~_)
  // : (hold(LA,attackHold(LA))~_)
  // : convexAttack(LA)
  // : (linAtt(LA-4)~_)
  // : (shapedAttack(LA)~_)
  // : (release(LA,holdTime(LA))~_)
  // (
  // (x:hold(LA,holdTime(LA))~_)
  // :slidingMax(holdTime(LA),lookahead(LA))@(lookahead(LA)-holdTime(LA))
  // )
  // , (x:hold(LA,holdTime(LA))~_)@(lookahead(LA))
  x@(2*lookahead(LA)+(0*lookahead(LA-4)))
, (
  x
  :(hold(LA,holdTime(LA))~_)
   // :slidingMax(holdTime(LA),lookahead(LA))@(lookahead(LA)-holdTime(LA))
  :(LArelease(LA,holdTime(LA))~_)
)
  // , (x: (hold(LA,attackHold(LA))~_): convexAttack(LA): (shapedAttack(LA)~_)@(0*lookahead(LA-4)  ))
;

holdTime(LA) = hslider("holdTime", lookahead(LA), 0, lookahead(LA), nrBlocks);
// attackHold(LA) = pow(2,attack*LA-1)*hslider("attackHold", 1, 0, 1, 0.01):int+1:max(0):min(lookahead(LA));
// attackHold(LA) = hslider("holdTime", lookahead(LA), 0, lookahead(LA), 1);
// attackHold(LA) = pow(2,attack*LA-1)*hslider("attackHold", 1, 0, 1, 0.01):int:hbargraph("hold",0,lookahead(LA)):max(0):min(lookahead(LA));
attackHold(LA) = pow(2,attack*LA-1)*hslider("attackHold", 1, 0, 2, 0.01):int:hbargraph("hold",0,lookahead(LA)):max(0):min(lookahead(LA));

// attack = hslider("[1]attack", 1, 0, 1, 0.01);
attack = hslider("[1]attack", 13, 0, 13, 0.1)/13;
// attack = hslider("[1]attack", 6, 0, 6, 1)/6;

LArelease(LA,holdTimeX,prevGain,x) =
  (par(i, nrBlocks,
       // (x:LAreleaseBlock(LA,holdTimeX/nrBlocks*(i+1):ceil+off:int:min(lookahead(LA):max(1)),prevGain))
       // (x:LAreleaseBlock(LA,holdTimeX/nrBlocks*(i+1)
       (x:LAreleaseBlock(LA,holdTimeX/nrBlocks*(i+1)
                            // : ceil
                            // :int+off
                            :min(lookahead(LA))
                             // :max(1)
                         ,prevGain))
      )
   :minOfN(nrBlocks)
  )
,
  (x:LAreleaseBlock(LA,holdTimeX)~_)
;
nrBlocks =32;
off =
  0;
// hslider("off", 0, -1, 8, 1);

LAreleaseBlock(LA,holdTime,prevGain,x) =
  minGain
  :getDir(LA,holdTime,prevGain,x)
   // , minGain
   // , x@lookahead(LA)
   // ,trig
with {
  minGain =
    // slidingMin(holdTime,lookahead(LA),x)@(lookahead(LA)-holdTime)
    x
    // :(hold(LA,holdTime)~_)
    // :(hold(LA,holdTime,prevGain))
    :
    slidingMax(holdTime,lookahead(LA),_)
    @(lookahead(LA)-holdTime)
    // :max(testSig(LA-5)@(2*lookahead(LA)))
  ;
  trig = prevGain>=x@(1*lookahead(LA));
  // diff = (minGain-prevGain);
  diff = (minGain:ba.sAndH(trig)-prevGain);
  // diff = (minGain-prevGain):ba.sAndH(trig);
  getDir(LA,holdTime,prevGain,x,minGain) =
    // (newDir~_)
    select2(trig
           ,(newDir~_)+prevGain
           ,(x@(1*lookahead(LA)))
           )
  with {
    newDir(prevDir) =
      select2(trig
             ,dir
              :max(prevDir)
             ,0-(ma.MAX));
    dir = diff/holdTime:max(0);
    // trig = diff<0;
  };
};

LAreleaseOLD(LA,holdTime,prevGain,x) =
  (newDir~_)+prevGain:min(x@lookahead(LA))//:max(minGain)
with {
  newDir(prevDir) =
    (prevGain-minGain)/holdTime
    :min(select2((prevGain+prevDir)<=minGain
                , prevDir
                  // , 1
                ,ma.MAX
                )
        );
  minGain = slidingMin(holdTime,lookahead(LA),x)@(lookahead(LA)-holdTime);
};


hold(LA,holdTime,prevGain,x) =
  slidingMin(holdTime,lookahead(LA),x)@(lookahead(LA)-holdTime)
  : max(prevGain)
  : min(x@lookahead(LA));

release(LA,holdTime,prevGain,x) =
  select2(
    diff>0
  , x
  , (prevGain+direction)
  )
with {
  diff=x-prevGain;
  dir = prevGain-prevGain';
  prevDir = dir';
  dirRel= (diff/relFactor);
  dirRelMin= (diff/relFactorMin);
  direction = xfade(keepDir,dirRel,prevDir):min(dirRelMin);
  relFactor = holdTime*hslider("release", 0.14, 0, 1, 0.01);
  relFactorMin = holdTime*hslider("releaseMin", 0.14, 0, 1, 0.01);
  keepDir = hslider("keepDir", 0, 0, 1, 0.01):pow(1/64);
  xfade(x,a,b) = (1-x)*a+x*b;
};


convexAttackAlsoBroken(LA,x) =
  (
    paramArray(attackConvexVal(LA),0,attackConvexBand(LA),0,LA)
    // : par(i, LA, hbargraph("hold%i",0,maxHold(LA)))
    // par(i, LA, holdT:hbargraph("hold%i",0,maxHold(LA+1)))
    // , (x<:si.bus(LA))
   ,( x:sequentialMinimumParOutDelayed(LA)
        // ,( x:sequentialMinimumParOut(LA)
      : (!,si.bus(LA))
    )
  )
  :ro.interleave(LA,2)
  :
  par(i, LA,
      (
        hold(LA)
        :
        linAttack(LA,i+1,x)~_
                            // :
                            // _ @(lookahead(LA)+1-(1<<(i+1)))
      )
     )
  :(par(i, LA-3, !),si.bus(3))
   // :maxOfN(LA)
with {
  holdT = hslider("holdT", 0, 0, lookahead(LA), 1);
  attackConvexBand(LA) = (attack*LA);
  attackConvexVal(LA) = pow(2,(attackConvexBand(LA)-3));
  selectn(maxN,N) = par(i, maxN, _*(i==N)):>_;
  hold(LA,holdTime,x) =
    x@maxHold(LA),(holdTime:!)
                  // slidingMin(HT(LA),maxHold(LA),x)@(maxHold(LA)-HT(LA))
  with {
    HT(LA) = holdTime:int:max(0):min(maxHold(LA));
  };
  maxHold(LA) = 1<<(LA)-1;
};

linAttack(LA,N,x,prevGain,minGain) =
  (minGain:getDirection(N,prevGain))
  : (getGain(LA,x,prevGain))
with {
  // getGain(LA,x,prevGain,index,direction,minGain)=
  // getGain(LA,x,prevGain,direction,minGain)=
  // direction+ prevGain:min(0):min(x@lookahead(LA)):max(minGain)
  // ;
  getGain(LA,x,prevGain,direction)=
    select2(direction<0
           , x@(2*lookahead(LA))
           , (direction+ prevGain:min(0):min(x@(2*lookahead(LA))))
           )
  ;
};

convexAttackNICETRY(nrBlocks,LA,blockSize,x) =
  x@maxHold
, (sequentialBlockMinimumParOut(nrBlocks,lookahead(LA),x,blockSize)
   : (!,si.bus(nrBlocks))
   : delays
   : getGains
   : holds
     // : maxOfN(nrBlocks)
  )

with {
  delays = par(i, nrBlocks, _@(maxHold-((nrBlocks)*blockSize)));
  // delays = par(i, nrBlocks, _@(maxHold-((i+1)*blockSize)));
  // delays = par(i, nrBlocks+1, _@((nrBlocks-i)*blockSize));
  getGains = par(i, nrBlocks, gainIndex(i)~(_,_):(_,!));
  // 0= nrBlocks-1
  // 1=nrBlocks-2
  // 3=nrBlocks-3
  holds =
    par(j, nrBlocks,
        // _*maxcomp
        slidingMin(compSize(j),maxcomp)
       ) with {
    maxcomp = (nrBlocks-1)*lookahead(LA);
    compSize(j) = (nrBlocks-j-1)*blockSize;
  };

  maxHold = (nrBlocks)*lookahead(LA);
  gainIndex(i,prevGain,prevIndex,minGain) =
    // triggers :
    getGain(LA,x,prevGain,direction)
  , index
  with {
    // 3=0
    // 2=dif*1/2
    // 1=dif*3/4
    // 0=dif*7/8
    start =
      trig*startMult*dif
      // *(prevIndex==0)
    ;
    startMult = 1-pow(2,(-i));
    // startMult = 1-pow(2,(-nrBlocks+i));
    getGain(LA,x,prevGain,direction) =
      select2(direction<0
             , minGain
             , (direction+ prevGain + start
                // :min(0):min(x@lookahead(LA))
               )) ;
    index =
      select2(
        ((minGain:ba.sAndH(trig)-prevGain))<0
      , 0
      , select2(trig
               , ((prevIndex-1) :max(0))
               , blockI));
    direction =
      select2(trig
             ,  (minGain:ba.sAndH(trig)-prevGain) / ((prevIndex):max(1))
             , (minGain-prevGain)/(blockI+1))
      // * dirMult*pow(2,nrBlocks)
    ;
    trig = (proposedDirection<=(prevGain-prevGain'));
    proposedDirection = dif/(blockI+1);
    dif = minGain-prevGain;
    blockI = ((i+1)*blockSize);
  };
};

bottom(LA) = hslider("bottom", 0, 0, lookahead(LA), 1);
mid(LA) = hslider("mid", 0, 0, lookahead(LA), 1);
band(LA) = hslider("band", 0, 0, lookahead(LA), 1);
top(LA) = hslider("top", 0, 0, lookahead(LA), 1);

convexAttack(LA,x) =
  (
    paramArray(attackConvexVal(LA),0,attackConvexBand(LA),0,LA)
    // : par(i, LA, hbargraph("hold%i",0,maxHold(LA)))
    // par(i, LA, holdT:hbargraph("hold%i",0,maxHold(LA+1)))
  , (x<:si.bus(LA))
  )
  :ro.interleave(LA,2)
  :
  par(i, LA,
      (
        // (_,linAtt(i+1)~_)
        // : hold(LA)
        hold(LA) :
        (linAtt(i+1)~_)
        @(lookahead(LA)+1-(1<<(i+1)))
      )
     )
  :maxOfN(LA)
  : select2(attack<(2/LA),_,x@(2*lookahead(LA)))
with {
  holdT = hslider("holdT", 0, 0, lookahead(LA), 1);
  attackConvexBand(LA) = (attack*LA);
  attackConvexVal(LA) = pow(2,(attackConvexBand(LA)-3));
  selectn(maxN,N) = par(i, maxN, _*(i==N)):>_;
  hold(LA,holdTime,x) =
    // _
    slidingMin(HT(LA),maxHold(LA),x)@(maxHold(LA)-HT(LA))
  with {
    HT(LA) = holdTime:int:max(0):min(maxHold(LA));
  };
  maxHold(LA) = 1<<(LA)-1;
};


shapedAttack(LA,prevGain,x) =
  (x:sequentialMinimumParOutDelayed(LA)
   : (!,si.bus(LA))
   : par(i, LA, getDirection(i+1,prevGain))
  , attackArray(LA)
    : (ro.interleave(LA,2)
      )
    : (par(i, LA, *)
      )
    : minOfN(LA)
    : (getGain(LA,x,prevGain))
  )
with {
  getGain(LA,x,prevGain,direction)=
    select2(direction<0
           , x@lookahead(LA)
             // , hold(LA,attackHold(LA)+holdTime(LA):min(lookahead(LA)),prevGain,x)
           , (direction+ prevGain:min(0)
              :min(x@lookahead(LA))
             )
           )
    // : hold(LA,attackHold(LA),prevGain)
  ;
  // attackHold(LA) = pow(2,attack*LA-1)*hslider("attackHold", 1, 0, 1, 0.01):int:hbargraph("hold",0,lookahead(LA)):max(0):min(lookahead(LA));
  // attackHold(LA) = pow(2,attack*LA-1)*hslider("attackHold", 1, 0, 1, 0.01):int+1:max(0):min(lookahead(LA));
  // attackHold(LA) = hslider("holdTime", lookahead(LA), 0, lookahead(LA), 1);

};

// hold(LA,holdTime,prevGain,x) =
// slidingMin(holdTime,lookahead(LA),x)@(lookahead(LA)-holdTime)
// : max(prevGain)
// : min(x@lookahead(LA));

linAtt(0,prevGain,x) = linearAttack(0,x,prevGain);
linAtt(LA,prevGain,x) = linearAttack(LA,x,prevGain);

linearAttack(0,x,prevGain) = x;
linearAttack(LA,x,prevGain) =
  (x:sequentialMinimumParOutDelayed(LA)
   : (!,si.bus(LA))
   : par(i, LA, getDirection(i+1,prevGain))
   : minOfN(LA)
   : (getGain(LA,x,prevGain))
  )
with {
  getGain(LA,x,prevGain,direction)=
    select2(direction<0
           , x@lookahead(LA)
           , (direction+ prevGain:min(0):min(x@lookahead(LA)))
           )
  ;
};


getDirection(LA,prevGain,minGain) =
  (getIndexAndDirection~(_,!))
  : (!,_)
    // : (_,_,minGain)
with {
  getIndexAndDirection(prevIndex) =
    index,direction
  with {
  index =
    select2(
      ((minGain:ba.sAndH(trig)-prevGain))<0
      // proposedDirection<0
    , 0
    , select2(trig
             , ((prevIndex-1) :max(0))
             , lookahead(LA)));
  direction =
    select2(trig
           ,  (minGain:ba.sAndH(trig)-prevGain) / ((prevIndex):max(1))
           , (minGain-prevGain)/(lookahead(LA)+1)) ;
  trig = (proposedDirection<=(prevGain-prevGain'));
  proposedDirection = (dif/(lookahead(LA)+1));
  dif = minGain-prevGain;
};
};


sequentialMinimumParOutDelayed(N) =
  sequentialOperatorParOut(N,min)
  : par(i, N+1, _@( 1<<N - 1<<i ));

sequentialMinimumParOut(N) =
  sequentialOperatorParOut(N,min);

sequentialOperatorParOut(N,op) =
  // operator(2)
  seq(i, N, operator(i))
with {
  operator(i) =
    myBus(i)
  ,
    (_<:
     _ , op(_,_@(1<<i) )
    )
  ;
};
sequentialBlockMinimumParOut(N,maxBlock,x,blockSize) =
  sequentialBlockOperatorParOut(N,min,ma.INFINITY,maxBlock,x,blockSize);
// sequentialBlockOperatorParOut(N,min,blockSize);

sequentialBlockOperatorParOut(N,op,disabledVal,maxBlock,x,blockSize) =
  // operator(2)
  x:seq(i, N, operator(i))
with {
  operator(i) =
    myBus(i)
  ,
    (_<:
     _ , op(
       slidingReduce(op,blockSize,maxBlock,disabledVal)
     )
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
minOfN(1) = _;
minOfN(N) = opOfN(N,min);
maxOfN(N) = opOfN(N,max);

find_Nmin(1) = _,_;
find_Nmin(2) =  Ncomparator;
find_Nmin(N) = seq(i,N-2, (Ncomparator,si.bus(2*(N-i-2)))) : Ncomparator;
Ncomparator(x,n,y,m) = select2((x<=y),y,x), select2((x<=y),m,n); // compare integer-labeled signals

lookahead(LA) = (1<<LA)-1;

myBus(0) =
  0:!
;
myBus(i) = si.bus(i);

attackArray(LA) =
  paramArray(1.4,0,attack2band(LA+1) ,0,LA+1)
  : (par(i, LA, min(1)),!)
;

// attack2band(LA) = (attack*LA);
// attack2band(LA) = 2+(attack*(LA-2)):max(2):hbargraph("band",0,LA);
attack2band(LA) = 1+(attack*(LA-1)):max(1);
// attack2band(LA) = hslider("band", 0, 0, LA, 0.1);


testSig(LA) =
  // checkbox("tst")*-1;
  // button:ba.impulsify*-1;
  no.lfnoise0(lookahead(LA) *t * (no.lfnoise0(lookahead(LA)/2):max(0.1) )):pow(3)*(1-noiseLVL) +(no.lfnoise(rate):pow(3) *noiseLVL):min(0);
t= hslider("[7]time", 1, 0.01, 4, 0.01):pow(2);
noiseLVL = hslider("[8]noise level", 0, 0, 1, 0.01);
rate = hslider("[9]rate [scale:log]", 420, 10, 10000, 1);


slidingReduce(op,N,0,disabledVal) = _;
slidingReduce(op,N,maxN,disabledVal) =
  sequentialOperatorParOut(maxNrBits(maxN+1)-1,op)
  :
  par(i,maxNrBits(maxN+1)
      , _@sumOfPrevBlockSizes(i)
        : useVal(i)
     ) : combine(maxNrBits(maxN+1))
with {
  // The sum of all the sizes of the previous blocks
  sumOfPrevBlockSizes(0) = 0;
  sumOfPrevBlockSizes(i) = (ba.subseq((allBlockSizes),0,i):>_);
  allBlockSizes = par(i, maxNrBits(maxN), (pow2(i)) * isUsed(i));
  maxNrBits(n) = int2nrOfBits(n);

  // Apply <op> to <N> parallel input signals
  combine(2) = op;
  combine(N) = op(combine(N-1),_);

  // Decide wether or not to use a certain value, based on N
  // Basically only the second <select2> is needed,
  // but this version also works for N == 0
  // 'works' in this case means 'does the same as reduce'
  useVal(i) =
    // _ <: select2(
    // (i==0) & (N==0),
    select2(isUsed(i), disabledVal, _)
    // , _
    // )
  ;

  // useVal(i) =
  //     select2(isUsed(i), disabledVal,_);
  // isUsed(0,N,maxN) = 1;
  isUsed(i) = ba.take(i+1, (int2bin(N+1,maxN*2+1)));
  pow2(i) = 1<<i;
  // same as:
  // pow2(i) = int(pow(2,i));
  // but in the block diagram, it will be displayed as a number, instead of a formula

  // convert N into a list of ones and zeros
  int2bin(N,maxN) = par(j, maxNrBits(maxN), int(floor((N)/(pow2(j))))%2);
  // int2bin(N,maxN) = par(j, maxNrBits(maxN), int(floor((N)/(pow2(j))))%2);
  // calculate how many ones and zeros are needed to represent maxN
  int2nrOfBits(N) = int(floor(log(N)/log(2))+1);
};

slidingMin(n,maxn) = slidingReduce(min,n,maxn,ma.INFINITY);
slidingMax(n,maxn) = slidingReduce(max,n,maxn,0-ma.INFINITY);
