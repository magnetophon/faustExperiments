import("stdfaust.lib");
declare author "Julius Smith";


external = checkbox("[0]use external input");

freq = hslider("[1]freq [unit:hz] [scale:log]
	[tooltip: frequency of the test oscillator]", F0min, F0min, F0max, 0.001);
lp_freq = hslider("[2]lp_freq [unit:hz] [scale:log]
	[tooltip: frequency of the low pass filter on the test oscillator]", 20000, 20, 20000, 1);
hp_freq = hslider("[3]hp_freq [unit:hz] [scale:log]
	[tooltip: frequency of the high pass filter on the test oscillator]", 20, 20, 20000, 1);
noiseLevel = hslider("[4]noiseLevel", 0, 0, 1, 0.1);

level = hslider("[5]level (db)", -10, -70, 10, 0.1) : ba.db2linear : si.smooth(0.999);

medianBP = checkbox("[6]median bypass");
portamentoTime =	hslider("[7] Portamento [unit:sec] [scale:log]
	[tooltip: Portamento (frequency-glide) time-constant in seconds]",
	                        0,0.00,1,0.001);
pitchMeter = hbargraph("[8]pitch", F0min, F0max);

// Gate parameters:
thresh = -35.0; // dB
att = 0.005;   // attack time constant (sec) for gate to open
hold = 0.1;     // hold time = time (sec) gate stays open after signal < thresh
rel  = 0.02;     // release time = time-constant (sec) for gate to close
gate = ef.gate_gain_mono(thresh,att,hold,rel);


// F0min = 80.0;  // lowest tracked pitch (Hz)
// F0max = 500.0; // highest tracked pitch (Hz)
F0min = 300.0;  // lowest tracked pitch (Hz)
F0max = 2000.0; // highest tracked pitch (Hz)
// F0min = 430.0;  // for block diagram
// F0max = 450.0; // for block diagram

// SRguess = 44100.0; // Can't seem to use SR in expression below:
SRguess = 48000.0; // Can't seem to use SR in expression below:
MINPER = int(SRguess/F0max);
MAXPER = int(SRguess/F0min);
NLAGS = MAXPER-MINPER+1;

lag2hz(lag) = float(ma.SR)/float(lag);

// FIXME: should use rect smoothing window, not exp
// acor(L) = _ <: _,@(L) : * <: select3(hslider("exp,rectL,rectNLAGS", 0, 0, 2, 1), fi.pole(0.9999), ba.slidingSum(L*mult), ba.slidingSum(NLAGS*mult));
// acor(L) = _ <: _,@(L) : * <: select2(checkbox("rect"), fi.pole(0.9999), ba.slidingSum(NLAGS*mult));
JuliusAcor(L) = _ <: _,@(L) : * <: select2(checkbox("rect"), fi.pole(0.9999), _);
// Bitstream Autocorrelation
// https://www.cycfi.com/2018/03/fast-and-efficient-pitch-detection-bitstream-autocorrelation/
BitAcor(L) = int(zeroCross) <: _,@(L) : xnor <: select2(checkbox("rect"), fi.pole(0.9999), _);
zeroCross(x) = ((x>=0) & (x'<0)) | ((x>0) & (x'<=0));
xnor = 1-xor(_,_);
acor(L) = BitAcor(L);
// acor(L) = JuliusAcor(L);
mult = hslider("mult", 1, 1, 100, 0.1);

macor(MINP,MAXP) = _ <: par(i,NLAGS,(MINP+i,acor(MINP+i)));

lcomparator(n,x,m,y) = select2((x>y),m,n), select2((x>y),y,x); // compare integer-labeled signals

find_lmax(N) = seq(i,N-2, (lcomparator,si.bus(2*(N-i-2)))) : lcomparator;

median3p(a,b,c) = select3(s,b,a,c)
with {
  cf(a,b) = 2*(a<b)-1;
  s=abs(cf(a,b)+2*cf(b,c)+3*cf(c,a))/2;
};

median3(x) = median3p(x,x',x'');

portamento = ba.bypass1(medianBP, median3) : si.smooth(ba.tau2pole(portamentoTime)); // pitch filtering
// smooth(p) = (1-p)/(1-p/z)

pitchTracker = (macor(MINPER,MAXPER) : find_lmax(NLAGS) : lag2hz,!);


signal(x) = select2(external, oscillator,x );
oscillator = (os.sawtooth(freq), no.noise):si.interpolate(noiseLevel): fi.lowpass(3,lp_freq): fi.highpass(3,hp_freq)*0.5;

process(x) =
  signal(x)
, ((pitchTracker(signal(x)) : portamento : pitchMeter):os.sawtooth * level * gate(signal(x)): fi.lowpass(3,lp_freq): fi.highpass(3,hp_freq)*0.5)
;
