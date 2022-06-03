declare name "LazyLeveler";
declare version "0.1";
declare author "Bart Brouns";
declare license "GPLv3";

import("stdfaust.lib");

// N = hslider("maxN", 0, 0, maxN, 1);
// maxN = pow(2,4);
N = maxN;
maxN = 14;

process =
  // pow(2,32);
  // maxn;
  slidingMax(N,maxN);
// ba.slidingMax(N,maxN);

slidingMin(n,maxn) = slidingReduce(min,n,maxn,ma.INFINITY);
slidingMax(n,maxn) = slidingReduce(max,n,maxn,0-ma.INFINITY);






slidingReduce(op,N,0,disabledVal) = _;
slidingReduce(op,N,maxN,disabledVal) =
  sequentialOperatorParOut(maxNrBits(maxN+1)-1,op)
  :
  par(i,maxNrBits(maxN+1)
      , _@sumOfPrevBlockSizes(i)
        : useVal(i)
     ) : combine(maxNrBits(maxN+1))
with {
  sequentialOperatorParOut(N,op) =
    seq(i, N, operator(i));
  operator(i) =
    myBus(i)
  ,
    (_<:
     _ , op(_,_@(pow2(i) ))
    ) ;


  // todo:
  myBus(0) = 0:! ;
  myBus(i) = si.bus(i);
  // The sum of all the sizes of the previous blocks
  sumOfPrevBlockSizes(0) = 0;
  sumOfPrevBlockSizes(i) = (ba.subseq((allBlockSizes),0,i):>_);
  allBlockSizes = par(i, maxNrBits(maxN), (pow2(i)) * isUsed(i));
  maxNrBits(n) = int2nrOfBits(n);

  // Apply <op> to <N> parallel input signals
  combine(2) = op;
  combine(N) = op(combine(N-1),_);

  // Decide wether or not to use a certain value, based on N
  useVal(i) =
    select2(isUsed(i), disabledVal, _)
  ;

  isUsed(i) = ba.take(i+1, (int2bin(N+1,maxN*2+1)));
  pow2(i) = 1<<i;
  // same as:
  // pow2(i) = int(pow(2,i));
  // but in the block diagram, it will be displayed as a number, instead of a formula

  // convert N into a list of ones and zeros
  int2bin(N,maxN) = par(j, maxNrBits(maxN), int(floor((N)/(pow2(j))))%2);
  // calculate how many ones and zeros are needed to represent maxN
  int2nrOfBits(N) = int(floor(log(N)/log(2))+1);
};
