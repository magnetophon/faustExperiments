import("filter.lib"); // /l/fa/filter.lib
import("effect.lib"); // /l/fa/effect.lib
import("oscillator.lib"); // /l/fa/oscillator.lib

// Gate parameters:
thresh = -35.0; // dB
att = 0.005;   // attack time constant (sec) for gate to open
hold = 0.1;     // hold time = time (sec) gate stays open after signal < thresh
rel  = 0.02;     // release time = time-constant (sec) for gate to close
gate = gate_gain_mono(thresh,att,hold,rel);

level = hslider("level (db)", -10, -70, 10, 0.1) : db2linear : smooth(0.999);

F0min = 80.0;  // lowest tracked pitch (Hz)
F0max = 500.0; // highest tracked pitch (Hz)

SRguess = 44100.0; // Can't seem to use SR in expression below:
MINPER = int(SRguess/F0max);
MAXPER = int(SRguess/F0min);
NLAGS = MAXPER-MINPER+1;

lag2hz(lag) = float(SR)/float(lag);

acor(L) = _ <: _,@(L) : * : pole(0.9999);
// FIXME: should use rect smoothing window, not exp

macor(MINP,MAXP) = _ <: par(i,NLAGS,(MINP+i,acor(MINP+i)));

lcomparator(n,x,m,y) = select2((x>y),m,n), select2((x>y),y,x); // compare integer-labeled signals

find_lmax(N) = seq(i,N-2, (lcomparator,bus(2*(N-i-2)))) : lcomparator; 

median3p(a,b,c) = select3(s,b,a,c) 
with { 
  cf(a,b) = 2*(a<b)-1;
  s=abs(cf(a,b)+2*cf(b,c)+3*cf(c,a))/2; 
};

median3(x) = median3p(x,x',x'');

portamento = median3 : smooth(0.99); // pitch filtering
// smooth(p) = (1-p)/(1-p/z)

process = _ <: gate, 
	(macor(MINPER,MAXPER) : find_lmax(NLAGS) : lag2hz,! : portamento : 
        sawtooth : *(level)) : *;
