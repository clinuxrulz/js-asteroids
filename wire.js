// data Wire m a b = Wire (DT -> WValue a -> m (WValue b, Wire m a b))
// data WValue a = WVInhibited | WVUnchanged a | WVChanged a

// wvInhibited :: WValue
var wvInhibited = function(r) {
	return r.wvInhibited();
};

// wvUnchanged :: WValue
var wvUnchanged = function(a) {
	return function(r) {
		return r.wvUnchanged(a);
	};
};

// wvChanged :: WValue
var wvChanged = function(a) {
	return function(r) {
		return r.wvChanged(a);
	};
};

// Show a => Show (WValue a)
var wvShow = function(m) {
	return {
		show: function(a) {
			return a({
				wvInhibited: function() { return "inhibited"; },
				wvUnchanged: function(x) { return "unchanged " + m.show(x); },
				wvChanged: function(x) { return "changed " + m.show(x); }
			});
		}
	};
};

// Show Number
var numberShow = {
	show: function(a) {
		return "" + a;
	}
};

// data Event a = Event a | NoEvent

// wEvent :: a -> Event a
var wEvent = function(a) {
	return function(r) {
		return r.wEvent(a);
	};
};

// wNoEvent :: Event a
var wNoEvent = function(r) {
	return r.wNoEvent();
};

var unit = {};

var shallowCopy = function(oldObj) {
	var newObj = {};
	for (var i in oldObj) {
		if (oldObj.hasOwnProperty(i)) {
			newObj[i] = oldObj[i];
		}
	}
	return newObj;
};

function toArray(x) {
	return Array.prototype.slice.call(x);
};

function sub_curry(fn /*, variable number of args */) {
	var args = [].slice.call(arguments, 1);
	return function () {
		return fn.apply(this, args.concat(toArray(arguments)));
	};
}

function curry(fn, length) {
	// capture fn's # of parameters
	length = length || fn.length;
	return function () {
		if (arguments.length < length) {
			// not all arguments have been specified. Curry once more.
			var combined = [fn].concat(toArray(arguments));
			return length - arguments.length > 0 
				? curry(sub_curry.apply(this, combined), length - arguments.length)
				: sub_curry.call(this, combined );
		} else {
			// all arguments have been specified, actually call function
			return fn.apply(this, arguments);
		}
	};
}

var semigroup = function(def) {
	return def;
};

var monoid = function(def) {
	return def;
};

var functor = function(def) {
	return def;
};

var apply = function(def) {
	var map = def.map;
	var ap = def.ap;
	var r = shallowCopy(def);
	r.ap2 = curry(function(mf, ma, mb) {
		return r.ap(r.ap(mf, ma), mb);
	});
	r.ap3 = curry(function(mf, ma, mb, mc) {
		return r.ap(r.ap2(mf, ma, mb), mc);
	});
	r.apply2 = curry(function(f, ma, mb) {
		return r.ap(r.map(f, ma), mb);
	});
	r.apply3 = curry(function(f, ma, mb, mc) {
		return r.ap(r.apply2(f, ma, mb), mc);
	});
	r.apply4 = curry(function(f, ma, mb, mc, md) {
		return r.ap(r.apply3(f, ma, mb, mc), md);
	});
	r.seqLeft = curry(function(ma, mb) {
		return r.apply2(
			curry(function(a,b) {
				return a;
			}),
			ma,
			mb
		);
	});
	r.seqRight = curry(function(ma, mb) {
		return r.apply2(
			curry(function(a,b) {
				return b;
			}),
			ma,
			mb
		);
	});
	return r;
};

var applicative = function(def) {
	var pure = def.pure;
	var r = shallowCopy(def);
	// seq :: [m a] -> m [a]
	r.seq = function(a) {
		if (a.length == 0) {
			return r.pure([]);
		} else if (a.length == 1) {
			return r.map(function(x) { return [x]; }, a[0]);
		} else {
			var i = a.length / 2;
			return r.apply2(
				curry(function(x, x2) {
					return x.concat(x2);
				}),
				r.seq(a.splice(0, i)),
				r.seq(a.splice(i, a.length))
			);
		}
	};
	// seq_ :: [m a] -> m ()
	r.seq_ = function(a) {
		if (a.length == 0) {
			return r.pure(unit);
		} else if (a.length = 1) {
			return r.seqRight(a[0], r.pure(unit));
		} else {
			var i = a.length / 2;
			return r.seqRight(
				r.seq_(a.splice(0, i)),
				r.seq_(a.splice(i, a.length))
			);
		}
	};
	return r;
};

var bind = function(def) {
	var r = shallowCopy(def);
	// join :: m (m a) -> m a
	r.join = function(mma) {
		return r.bind(mma, function(x) { return x; });
	};
	return r;
};

var monad = function(def) {
	return def;
};

var semigroupoid = function(def) {
	var r = shallowCopy(def);
	var o = r.o;
	r.composeLeft = r.o;
	r.composeRight = curry(function(w1, w2) {
		return r.o(w2, w1);
	});
	return r;
};

var category = function(def) {
	return def;
};

var arrow = function(def) {
	var r = shallowCopy(def);
	var arr = r.arr;
	var first = r.first;
	var swap = r.arr(function(x) { return [x[1],x[0]]; });
	var dup = r.arr(function(x) { return [x,x]; });
	r.second = function(x) { return r.o(r.swap, r.o(first(x), r.swap)); };
	if (!r.hasOwnProperty('split')) {
		r.split = function(x1, x2) { return r.o(r.second(x2), r.first(x1)); };
	}
	r.fanout = function(x1, x2) { return r.o(r.split(x1,x2), dup); };
	r.o3 = function(w1,w2,w3) { return r.o(w1, r.o(w2, w3)); };
	r.o4 = function(w1,w2,w3,w4) { return r.o(r.o(w1, w2), r.o(w3, w4)); };
	r.o5 = function(w1,w2,w3,w4,w5) { return r.o(r.o(w1, w2), r.o3(w3, w4, w5)); };
	return r;
};

var array = (function() {
	return monoid(semigroup({
		// zero :: [a]
		zero: [],
		// sum :: [a] -> [a] -> [a]
		sum: curry(function(x1, x2) {
			return x1.concat(x2);
		})
	}));
})();

var lazy = (function() {
	return monad(applicative(bind(apply(functor({
		// map :: (a -> b) -> Lazy a -> Lazy b
		map: function(f, ma) {
			return function() {
				return f(ma());
			};
		},
		// ap :: Lazy (a -> b) -> Lazy a -> Lazy b
		ap: function(mf, ma) {
			return function() {
				var f = mf();
				var a = ma();
				return f(a);
			};
		},
		// bind :: Lazy a -> (a -> Lazy b) -> Lazy b
		bind: function(ma, f) {
			return function() {
				return f(ma())();
			};
		},
		// pure: a -> Lazy a
		pure: function(a) {
			return function() {
				return a;
			};
		}
	})))));
})();

var io = (function() {
	return monad(applicative(bind(apply(functor({
		// map :: (a -> b) -> IO a -> IO b
		map: function(f, ma) {
			return function() {
				return f(ma());
			};
		},
		// ap :: IO (a -> b) -> IO a -> IO b
		ap: function(mf, ma) {
			return function() {
				var f = mf();
				return f(ma());
			};
		},
		// bind :: IO a -> (a -> IO b) -> IO b
		bind: function(ma, f) {
			return function() {
				return f(ma())();
			};
		},
		// pure :: a -> IO a
		pure: function(a) {
			return function() {
				return a;
			};
		},
		// mfix :: (Lazy a -> IO a) -> IO a
		mfix: function(f) {
			return function() {
				return f(function() {
					return io.mfix(f)();
				})();
			};
		},
		// instance Semigroup a => Semigroup (IO a)
		semigroup: function(m) {
			return {
				sum: curry(function(ma, mb) {
					return io.apply2(
						curry(function(a,b) { return m.sum(a,b); }),
						ma,
						mb
					);
				})
			};
		},
		// instance Monoid a => Monoid (IO a)
		monoid: function(m) {
			var r = io.semigroup(m);
			r.zero = io.pure(m.zero);
			return r;
		}
	})))));
})();

var id = (function() {
	return monad(applicative(bind(apply(functor({
		map: function(f, ma) {
			return f(ma);
		},
		ap: function(mf, ma) {
			return mf(ma);
		},
		bind: function(ma, f) {
			return f(ma);
		},
		pure: function(a) {
			return a;
		}
	})))));
})();

var wv = apply(functor({
	// map :: (a -> b) -> WValue a -> WValue b
	map: function(f, x) {
		return x({
			wvInhibited: function() {
				return wvInhibited;
			},
			wvUnchanged: function(a) {
				return wvUnchanged(f(a));
			},
			wvChanged: function(a) {
				return wvChanged(f(a));
			}
		});
	},
	// ap :: WValue (a -> b) -> WValue a -> WValue b
	ap: function(mf, ma) {
		return mf({
			wvInhibited: function() {
				return wvInhibited;
			},
			wvUnchanged: function(f) {
				return ma({
					wvInhibited: function() {
						return wvInhibited;
					},
					wvUnchanged: function(a) {
						return wvUnchanged(f(a));
					},
					wvChanged: function(a) {
						return wvChanged(f(a));
					}
				});
			},
			wvChanged: function(f) {
				return ma({
					wvInhibited: function() {
						return wvInhibited;
					},
					wvUnchanged: function(a) {
						return wvChanged(f(a));
					},
					wvChanged: function(a) {
						return wvChanged(f(a));
					}
				});
			}
		});
	}
}));

// data Wire m a b = Wire (WValue a -> m (WValue b, Wire m a b))
// data WValue a = WVInhibited | WVUnchanged a | WVChanged a
var wire = function(m) {
	var wire_m = applicative(apply(functor(arrow(category(semigroupoid({
		// mkGen :: (Double -> a -> m (WValue b, Wire m a b)) -> Wire m a b
		mkGen: function(x) {
			return function(dt, x2) {
				return x2({
					wvInhibited: function() {
						return m.pure([wvInhibited, wire_m.mkGen(x)]);
					},
					wvUnchanged: function(a) {
						return x(dt, a);
					},
					wvChanged: function(a) {
						return x(dt, a);
					}
				});
			};
		},
		// map :: (a -> b) -> Wire m r a -> Wire m r b
		map: function(f, w) {
			return wire_m.o(wire_m.arr(f), w);
		},
		// ap :: Wire m r (a -> b) -> Wire m r a -> Wire m r b
		ap: function(w1, w2) {
			return wire_m.o(
				wire_m.arr(function(x) {
					return x[0](x[1]);
				}),
				wire_m.fanout(w1, w2)
			);
		},
		// pure :: a -> Wire m r a
		pure: function(a) {
			var pure2 = function(dt, x) {
				return m.pure(x({
					wvInhibited: function() {
						return [wvInhibited, pure2];
					},
					wvUnchanged: function(x2) {
						return [wvUnchanged(a), pure2];
					},
					wvChanged: function(x2) {
						return [wvUnchanged(a), pure2];
					}
				}));
			};
			return function(dt, x) {
				return m.pure(x({
					wvInhibited: function() {
						return [wvInhibited, pure(a)];
					},
					wvUnchanged: function(x2) {
						return [wvChanged(a), pure2];
					},
					wvChanged: function(x2) {
						return [wvChanged(a), pure2];
					}
				}));
			};
		},
		// o :: Wire m b c -> Wire m a b -> Wire m a c
		o: function(w1, w2) {
			return function(dt, x) {
				return m.bind(
					w2(dt, x),
					function(x2) {
						return m.map(
							function(x3) {
								return [x3[0], wire_m.o(x3[1], x2[1])];
							},
							w1(dt, x2[0])
						);
					}
				);
			};
		},
		// id :: Wire m a a
		id: function(dt, x) {
			return m.pure([x,wire_m.id]);
		},
		// arr :: (a -> b) -> Wire m a b
		arr: function(f) {
			var arr2 = function(b) {
				return function(dt, x) {
					return m.pure(x({
						wvInhibited: function() {
							return [wvInhibited,arr2(b)];
						},
						wvUnchanged: function(a) {
							return [wvUnchanged(b),arr2(b)];
						},
						wvChanged: function(a) {
							var b = f(a);
							return [wvChanged(b),arr2(b)];
						}
					}));
				};
			};
			return function(dt, x) {
				return m.pure(x({
					wvInhibited: function() {
						return [wvInhibited,wire_m.arr(m, f)];
					},
					wvUnchanged: function(a) {
						var b = f(a);
						return [wvChanged(b),arr2(b)];
					},
					wvChanged: function(a) {
						var b = f(a);
						return [wvChanged(b),arr2(b)];
					}
				}));
			};
		},
		// split :: Wire m a1 b1 -> Wire m a2 b2 -> Wire m (a1,a2) (b1,b2)
		split: function(w1, w2) {
			return function(dt, x) {
				return m.apply2(
					curry(function(x2, x3) {
						var w3 = wire_m.split(x2[1], x3[1]);
						return x2[0]({
							wvInhibited: function() {
								return [wvInhibited, w3];
							},
							wvUnchanged: function(a1) {
								return x3[0]({
									wvInhibited: function() {
										return [wvInhibited, w3];
									},
									wvUnchanged: function(a2) {
										return [wvUnchanged([a1,a2]), w3];
									},
									wvChanged: function(a2) {
										return [wvChanged([a1,a2]), w3];
									}
								});
							},
							wvChanged: function(a1) {
								return x3[0]({
									wvInhibited: function() {
										return [wvInhibited, w3];
									},
									wvUnchanged: function(a2) {
										return [wvChanged([a1,a2]), w3];
									},
									wvChanged: function(a2) {
										return [wvChanged([a1,a2]), w3];
									}
								});
							}
						});
					}),
					w1(dt, wv.map(function(x2) { return x2[0]; }, x)),
					w2(dt, wv.map(function(x2) { return x2[1]; }, x))
				);
			};
		},
		// first :: Wire m a b -> Wire m (a,c) (b,c)
		first: function(w) {
			return wire_m.split(w, wire_m.id);
		},
		// integral :: a -> Wire m a a
		integral: function(a) {
			return function(dt, x) {
				var unchangedOrChangedHandler = function(x2) {
					if (dt == 0.0) {
						return [wvUnchanged(a), wire_m.integral(a)];
					} else {
						var a2 = a + x2 * dt;
						return [wvChanged(a2), wire_m.integral(a2)];
					}
				};
				return m.pure(x({
					wvInhibited: function() {
						return [wvInhibited, wire_m.integral(a)];
					},
					wvUnchanged: unchangedOrChangedHandler,
					wvChanged: unchangedOrChangedHandler
				}));
			};
		},
		// integralWith :: (w -> a -> a) -> a -> Wire m (a,w) a
		integralWith: function(f, a) {
			return function(dt, x) {
				var unchangedOrChangedHandler = function(x2) {
					var a2 = f(x2[1], a + x2[0] * dt);
					return [wvChanged(a2), wire_m.integralWith(f, a2)];
				};
				return m.pure(x({
					wvInhibited: function() {
						return [wvInhibited, wire_m.integralWith(f, a)];
					},
					wvUnchanged: unchangedOrChangedHandler,
					wvChanged: unchangedOrChangedHandler
				}));
			};
		},
		// inhibit :: Wire m a b
		inhibit: function(dt, x) {
			return m.pure([wvInhibited, wire_m.inhibit]);
		},
		// for :: t -> Wire m a a
		for_: function(t) {
			return function(dt, x) {
				if (t <= 0.0) {
					return m.pure([wvInhibited, wire_m.inhibit]);
				} else {
					return m.pure([x, wire_m.for_(t - dt)]);
				}
			};
		},
		// next :: Wire m a b -> Wire m a b -> Wire m a b
		next: function(w1, w2) {
			return function(dt, x) {
				return m.bind(
					w1(dt, x),
					function(x2) {
						return x2[0]({
							wvInhibited: function() {
								return w2(dt, x);
							},
							wvUnchanged: function(a) {
								return m.pure([x2[0], wire_m.next(x2[1], w2)]);
							},
							wvChanged: function(a) {
								return m.pure([x2[0], wire_m.next(x2[1], w2)]);
							}
						});
					}
				);
			};
		},
		// never :: Wire m a (Event a)
		never: function(dt, x) {
			var never2 = function(dt, x) {
				return m.pure([wvUnchanged(wNoEvent),never2]);
			};
			return m.pure([wvChanged(wNoEvent),never2]);
		},
		// now :: Wire m a (Event a)
		now: function(dt, x) {
			return m.pure(x({
				wvInhibited: function() {
					return [wvInhibited, wire_m.now];
				},
				wvUnchanged: function(a) {
					return [wvChanged(wEvent(a)), wire_m.never];
				},
				wvChanged: function(a) {
					return [wvChanged(wEvent(a)), wire_m.never];
				}
			}));
		},
		// hold :: Wire m (Event a) a
		hold: function(dt, x) {
			var hold2 = function(last) {
				return function(dt, x) {
					var unchangedOrChangedHandler = function(a) {
						return a({
							wEvent: function(a2) {
								return [wvChanged(a2), hold2(a2)];
							},
							wNoEvent: function() {
								return [wvUnchanged(last), hold2(last)];
							}
						});
					};
					return m.pure(x({
						wvInhibited: function() {
							return [wvInhibited, hold2(last)];
						},
						wvUnchanged: unchangedOrChangedHandler,
						wvChanged: unchangedOrChangedHandler
					}));
				};
			};
			var unchangedOrChangedHandler = function(a) {
				return a({
					wEvent: function(a2) {
						return [wvChanged(a2), hold2(a2)];
					},
					wNoEvent: function() {
						return [wvInhibited, wire_m.hold];
					}
				});
			};
			return m.pure(x({
				wvInhibited: function() {
					return [wvInhibited, wire_m.hold];
				},
				wvUnchanged: unchangedOrChangedHandler,
				wvChanged: unchangedOrChangedHandler
			}));
		},
		// when :: (a -> Boolean) :: Wire m a a
		when: function(fn) {
			var unchangedOrChangedHandler = function(a) {
				if (fn(a)) {
					return [wvChanged(a), wire_m.when(fn)];
				} else {
					return [wvInhibited, wire_m.when(fn)];
				}
			};
			return function(dt, x) {
				return m.pure(x({
					wvInhibited: function() {
						return [wvInhibited, wire_m.when(fn)];
					},
					wvUnchanged: unchangedOrChangedHandler,
					wvChanged: unchangedOrChangedHandler
				}));
			};
		},
		// delay :: a -> Wire m a a
		delay: function(x) {
			var delayUnchanged = function(x) {
				return function(dt, x2) {
					return m.pure(x2({
						wvInhibited: function() {
							return [wvInhibited, delayUnchanged(x)];
						},
						wvUnchanged: function(a) {
							return [wvUnchanged(x), delayUnchanged(a)];
						},
						wvChanged: function(a) {
							return [wvUnchanged(x), wire_m.delay(a)];
						}
					}));
				};
			};
			return function(dt, x2) {
				return m.pure(x2({
					wvInhibited: function() {
						return [wvInhibited, wire_m.delay(x)];
					},
					wvUnchanged: function(a) {
						return [wvChanged(x), delayUnchanged(a)];
					},
					wvChanged: function(a) {
						return [wvChanged(x), wire_m.delay(a)];
					}
				}));
			};
		},
		// loop :: Wire m (a,Lazy c) (b,Lazy c) -> Wire m a b
		loop: function(w) {
			return function(dt, x) {
				return m.map(
					function(x2) {
						return [
							wv.map(
								function(x3) { return x3[0]; },
								x2[0]
							),
							wire_m.loop(x2[1])
						];
					},
					// mfix :: (Lazy a -> m a) -> m a
					m.mfix(function(x2) {
						var lazyC = lazy.bind(
							x2,
							function(x3) {
								return x3[0]({
									wvInhibited: function() {
										throw "Inhibited in loop!";
									},
									wvUnchanged: function(a) {
										return a[1];
									},
									wvChanged: function(a) {
										return a[1];
									}
								});
							}
						);
						return w(
							dt,
							wv.map(
								function(x3) { return [x3,lazyC]; },
								x
							)
						);
					})
				);
			};
		}
	}))))));
	return wire_m;
};
