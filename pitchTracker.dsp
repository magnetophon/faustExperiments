import("stdfaust.lib");

BufferLength=300;

pitch(x) =
  // i
  ma.SR / max(M, 1) - ma.SR * (M == 0)
with {
  AC(x, k) = sum(j, BufferLength, (x@(j))*(x@(j+k))) / (BufferLength) ; // Correlating two signals, on the buffer length, ideally over 600 samples with actual SR
  // i = +(1) ~_ ;
  i = +(1) ~_ :min(1e9):max(0);
  // i = min(1e8,+(1)) ~_ ;
  // i = min(1e3,+(1):max(0)) ~_ ;
  // i = min(1e8,+(1)) ~_ ;
  U = AC(x,i)  >  AC(x,0)/2;             // Detection of a good correlation, AC(x,0) being the highest possible value, so its half becomes the detection threshold
  V = AC(x,i)  <= AC(x,i+1);
  W = AC(x,i+1) > AC(x,i+2);
  Y = U: *(V) : *(W);

  N = (+(1) : *(1 - Y)) ~ _;
  M = (N' + 1) : ba.sAndH(Y);
};

process = pitch : hbargraph("Frequency", 50, 2000): os.sawtooth ;
