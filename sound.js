// noise :: Monad m => Wire m a Double
var noise = function(wire_m, m) {
	var noise2 = function(n) {
		var nextN = ((n >> 1) | (((n & 1) ^ ((n ^ 2) >> 1)) << 14)) & 32767;
		return function(dt,x) {
			return m.pure([wvChanged(n),noise2(nextN)]);
		};
	};
	return wire_m.map(
		function(x) {
			return x / 16384.0 - 1.0;
		},
		noise2(2)
	);
};

// quantize :: Monad m => Wire m (Double,Double) Double
var quantize = function(wire_m, m) {
	return wire_m.arr(function(a) {
		return a[1] * Math.floor(a[0] / a[1]);
	});
};

/*
--------------------------------------------------------------------------------
-- | Decay a single over a period of time, inhibiting after.
*/
// decay :: Monad m => Double -> Wire m Double Double
var decay = function(wire_m, m, duration) {
	return wire_m.apply2(
		curry(function(a,b) { return a*b; }),
		wire_m.id,
		wire_m.o(
			wire_m.when(function(a) { return a>0.0; }),
			wire_m.o(
				wire_m.integral(1),
				wire_m.pure(-1.0 / duration)
			)
		)
	);
};

/*
--------------------------------------------------------------------------------
-- | Gate a signal to only pass above a certain threshold. Left input is the
-- signal to gate, right input is the gate threshold.
*/
// gate :: (Monad m, Num a, Ord a) => Wire m (a, a) a
var gate = function(wire_m, m) {
	return wire_m.map(
		function(a) {
			if (a[0] > a[1]) {
				return a[0];
			} else {
				return 0.0;
			}
		},
		wire_m.id
	);
};

/*
--------------------------------------------------------------------------------
-- | Reduce sampling frequency. Left input is the signal, right input is
-- required sample rate.
*/
// rateReduce :: Monad m => Wire m (Double, Int) Double
var rateReduce = function(wire_m, m) {
	var rateReduce2 = function(v, t) {
		return function(dt, x) {
			if (t > 0.0) {
				return m.pure([wvUnchanged(v), rateReduce2(v, t - dt)]);
			} else {
				return rateReduce(wire_m, m)(dt, x);
			}
		};
	};
	var unchangedOrChangedHandler = function(a) {
		return [wvChanged(a[0]), rateReduce2(a[0], 1.0 / a[1])];
	};
	return function(dt, x) {
		return m.pure(x({
			wvInhibited: function() {
				return [wvInhibited, rateReduce(m)];
			},
			wvUnchanged: unchangedOrChangedHandler,
			wvChanged: unchangedOrChangedHandler
		}));
	};
};

// explosion :: Monad m => Wire m a Double
var explosion = function(wire_m, m) {
	return wire_m.o5(
		decay(wire_m, m, 2.0),
		gate(wire_m, m),
		wire_m.fanout(rateReduce(wire_m, m), wire_m.pure(0.4)),
		wire_m.fanout(quantize(wire_m, m), wire_m.pure(500)),
		wire_m.fanout(noise(wire_m, m), wire_m.pure(0.2))
	);
};

// sin :: Monad m => Wire m Double Double
var sin = function(wire_m, m) {
	var sin2 = function(theta) {
		return function(dt, x) {
			var unchangedOrChangedHandler = function(frequency) {
				var theta2 = theta + 2.0 * Math.PI * frequency * dt;
				return [wvChanged(Math.sin(theta2)), sin2(theta2)];
			};
			return m.pure(x({
				wvInhibited: function() {
					return [wvInhibited, sin2(theta)];
				},
				wvUnchanged: unchangedOrChangedHandler,
				wvChanged: unchangedOrChangedHandler
			}));
		};
	};
	return sin2(0);
};

// death :: Monad m => Wire m a Double
var death = function(wire_m, m) {
	return wire_m.o5(
		decay(wire_m, m, 5.0),
		gate(wire_m, m),
		wire_m.fanout(rateReduce(wire_m, m), wire_m.pure(0.3)),
		wire_m.fanout(quantize(wire_m, m), wire_m.pure(3000)),
		wire_m.fanout(noise(wire_m, m), wire_m.pure(0.2))
	);
};

// clamp :: Monad m => Wire m Double Double
var clamp = function(wire_m, m) {
	return wire_m.arr(function(a) {
		if (a < -1.0) {
			return -1.0;
		} else if (a > 1.0) {
			return 1.0;
		} else {
			return a;
		}
	});
};

// pew :: Monad m => Wire m a Double
var pew = function(wire_m, m) {
	var sin_drop = wire_m.o(
		sin(wire_m, m),
		wire_m.map(
			function(a) { return a + 100.0; },
			wire_m.o(
				decay(wire_m, m, 0.5),
				wire_m.pure(600.0)
			)
		)
	);
	return wire_m.o3(
		decay(wire_m, m, 0.5),
		rateReduce(wire_m, m),
		wire_m.fanout(
			wire_m.map(
				function(a) { return a / 2.0; },
				wire_m.o(
					clamp(wire_m, m),
					wire_m.map(
						function(a) { return a * 1000.0; },
						sin_drop
					)
				)
			),
			wire_m.pure(5000.0)
		)
	);
};

// ufo :: Monad m => Wire m a Double
var ufo = function(wire_m, m) {
	return wire_m.o3(
		wire_m.for_(1.0 / 3.0),
		sin(wire_m, m),
		wire_m.map(
			function(x) {
				return x * 200.0 + 100.0;
			},
			wire_m.o(
				sin(wire_m, m),
				wire_m.pure(3.0)
			)
		)
	);
};

// render :: audioCtx -> Wire IO a Double -> IO Buffer
var render = function(audioCtx, w) {
	return function() {
		var sampleRate = 5000.0;
		var dt = 1.0 / sampleRate;
		var numSamples = 0;
		var tmp = w;
		var inhibited;
		var i;
		var y;
		var buffer;
		var data;
		while (true) {
			tmp = tmp(dt, wvChanged(unit))();
			inhibited = tmp[0]({
				wvInhibited: function() {
					return true;
				},
				wvUnchanged: function(a) {
					return false;
				},
				wvChanged: function(a) {
					return false;
				}
			});
			if (inhibited) { break; }
			tmp = tmp[1];
			++numSamples;
		}
		buffer = audioCtx.createBuffer(1, numSamples, sampleRate);
		data = buffer.getChannelData(0);
		tmp = w;
		for (i = 0; i < numSamples; ++i) {
			tmp = tmp(dt, wvChanged(unit))();
			y = tmp[0]({
				wvInhibited: function() {
					return 0;
				},
				wvUnchanged: function(a) {
					return a;
				},
				wvChanged: function(a) {
					return a;
				}
			});
			data[i] = y;
			tmp = tmp[1];
		}
		return buffer;
	};
};

var play = function(audioCtx, buffer) {
	return function() {
		var source = audioCtx.createBufferSource();
		source.buffer = buffer;
		source.connect(audioCtx.destination);
		source.start();
	};
};
