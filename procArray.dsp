import("stdfaust.lib");

procArray(N, processor, isLinear, loParams, hiParams) = paramArray:ro.interleave(N, K):par(i, N, processor)
    with {
        K = inputs(processor);
        // broadcast a single flag to K copies; leave a K-wide flag untouched
        // flags = ba.if(outputs(isLinear)==1, par(i, K, isLinear), isLinear);
        flags = broadcast(outputs(isLinear))
            with {
                broadcast = case{(1)=>par(i, K, isLinear);
                (n)=>isLinear;
                };
            };
        paramArray = (loParams, hiParams):ro.interleave(K, 2):par(i, K, oneArray(ba.take(i+1, flags)));
        oneArray = case{(0)=>logArray;
        (1)=>linArray;
        };
        linArray(lo, hi) = par(i, N, ((hi-lo)*frac(i))+lo);
        logArray(lo, hi) = par(i, N, lo*pow(hi/lo, frac(i)));
        frac(i) = i/(N-1);
    };

process = procArray(16,
    smoother,
    (0, 1, 0),
    (smallMult, smallOrder, _),
    (bigMult, bigOrder, _));

smoother(order, att) = smootherARorder(8, order, order, att, att);

smootherARorder(maxOrder, orderAtt, orderRel, att, rel, xx) = xx:seq(i, maxOrder, loop(i)~_)
    with {
        loop(i, fb, x) = coeff(i)*fb+(1.0-coeff(i))*x
            with {
                cutoffCorrection(order) = 1.0/sqrt(pow(2.0, 1.0/order)-1.0);
                // Wired for GAIN REDUCTION: GR ducks DOWNWARD on attack, so a FALLING
                // input (x < fb) selects attCoeff (the att param) and a RISING input
                // selects relCoeff (rel param). This is the swap that makes att = GR
                // attack and rel = GR release. (A standard amplitude-envelope follower
                // would use x > fb here instead.)
                coeff(i) = ba.if(x<fb, attCoeff(i), relCoeff(i));
                attCoeff(i) = exp(-TWOPIT*cutoffCorrection(orderAtt)/max(ma.EPSILON, att))*(i<orderAtt);
                relCoeff(i) = exp(-TWOPIT*cutoffCorrection(orderRel)/max(ma.EPSILON, rel))*(i<orderRel);
                TWOPIT = 2*ma.PI*ma.T;
            };
    };

MainGroup(x) = hgroup("[0]shapedSmoother", x);
TestGroup(x) = vgroup("[0]Test signal", x);
SmootherGroup(x) = vgroup("[1]Smoother", x);
// Endpoint subgroups: the big block (largest window) and small block (smallest window)
// each get their own mult/order/self-correct. Every window in between interpolates.
BigGroup(x) = SmootherGroup(hgroup("[1]Big block (largest window)", x));
SmallGroup(x) = SmootherGroup(hgroup("[2]Small block (smallest window)", x));
mult = hslider("[0]mult [x base att]", 1.5, 0.5, 4.0, 0.05);
order = hslider("[1]order", 8, 1, 8, 1);
SC = hslider("[2]self-correct", 0.0, 0.0, 1.0, 0.01);

bigMult = BigGroup(mult);
bigOrder = BigGroup(order);
bigSC = BigGroup(SC);
smallMult = SmallGroup(mult);
smallOrder = SmallGroup(order);
smallSC = SmallGroup(SC);
