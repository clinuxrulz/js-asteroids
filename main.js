var constants = {
	enableSound: true,
	turnLeft: 37,   // left arrow
	turnRight: 39,  // right arrow
	accelerate: 38, // up arrow
	fire: 32,       // space key
	turnSpeed: 270.0,
	acceleration: 30.0,
	fireRate: 5,       // bullets per second
	bulletSize: 5,
	bulletSpeed: 500.0,
	bulletLifespan: 5,   // seconds
	asteroids: [
		// smallest
		{
			speed: { min: 40, max: 50 },
			numVertices: { min: 3, max: 5 },
			radius: { min: 10, max: 25 }
		},
		// larger
		{
			speed: { min: 30, max: 40 },
			numVertices: { min: 5, max: 8 },
			radius: { min: 30, max: 40 }
		},
		// larger
		{
			speed: { min: 20, max: 30 },
			numVertices: { min: 10, max: 12},
			radius: { min: 60, max: 70 }
		},
		// largest
		{
			speed: { min: 15, max: 25 },
			numVertices: { min: 18, max: 25 },
			radius: { min: 60, max: 100 }
		}
	]
};

// data Ship = Ship {
//   id :: Integer,
//   svg :: Svg,
//   position :: (Double,Double)
//   rotation :: Double
// }
//
// data Asteroid = Asteroid {
//   id :: Integer,
//   svg :: Svg,
//   points :: [(Double,Double)],
//   position :: (Double,Double),
//   rotation :: Double
// }
//
// data Bullet = Bullet {
//   id :: Integer,
//   svg :: Svg,
//   position :: Double
// }
//
// data GameState = GameState {
//   ship :: Ship,
//   asteroids :: [Asteroid],
//   bullets :: [Bullet]
// }

var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
var explosionBuffer;
var ufoBuffer;
var deathBuffer;
var pewBuffer;
var svg;
var svgShipAngle;
var svgShipPosition;
var shipPosition;
var shipDirection;
var freeIds = [];
var nextId = 1;
var commandWires;

var allocIdIO = function() {
	var id;
	if (freeIds.length != 0) {
		id = freeIds[freeIds.length-1];
		freeIds.splice(freeIds.length-1,1);
	} else {
		id = nextId++;
	}
	return id;
};

var freeIdIO = function(id) {
	return function() {
		freeIds.push(id);
		return unit;
	};
};

// gameStateWire :: Wire m Input GameState
var gameStateWire = function(wire_m, m) {

};

// collisionCommandWire :: Wire m GameState Output
var collisionCommandWire = function(wire_m, m) {
	return wire_m.arr(function(gameState) {
		var outputs = [];
		var i, j;
		for (i = 0; i < gameState.asteroids.length; ++i) {
			var asteroid = gameState.asteroids[i];
			for (j = 0; j < gameState.bullets.length; ++j) {
				var bullet = gameState.bullets[j];
				if (isPointInPoly(asteroid.points, bullet.position)) {
					outputs.push({"type": "destroyAsteroid", "value": asteroid.id});
					outputs.push({"type": "destroyBullet", "value": bullet.id});
				}
			}
		}
		if (outputs.length == 0) {
			return {"type": "none"};
		} else if (outputs.length == 1) {
			return outputs[0];
		} else {
			return {"type": "seq", "value": outputs};
		}
	});
};

// keyDownWire :: Applicative m => KeyCode -> Wire m Input Bool
var keyDownWire = function(m, keyCode) {
	var keyDownWire2 = function(wasDown) {
		return function(dt, x) {
			return m.pure(x({
				wvInhibited: function() {
					return [wvInhibited,keyDownWire2(wasDown)];
				},
				wvUnchanged: function(a) {
					return [wvUnchanged(wasDown),keyDownWire2(wasDown)];
				},
				wvChanged: function(a) {
					if (wasDown && a.type == "keyUp" && a.keyCode == keyCode) {
						return [wvChanged(false), keyDownWire2(false)];
					} else if (!wasDown && a.type == "keyDown" && a.keyCode == keyCode) {
						return [wvChanged(true), keyDownWire2(true)];
					} else {
						return [wvUnchanged(wasDown),keyDownWire2(wasDown)];
					}
				}
			}));
		};
	};
	return function(dt, x) {
		return m.pure(x({
			wvInhibited: function() {
				return [wvInhibited,keyDownWire(m,keyCode)];
			},
			wvUnchanged: function(a) {
				if (a.type == "keyDown" && a.keyCode == keyCode) {
					return [wvChanged(true), keyDownWire2(true)];
				} else {
					return [wvChanged(false), keyDownWire2(false)];
				}
			},
			wvChanged: function(a) {
				if (a.type == "keyDown" && a.keyCode == keyCode) {
					return [wvChanged(true), keyDownWire2(true)];
				} else {
					return [wvChanged(false), keyDownWire2(false)];
				}
			}
		}));
	};
};

var shipTurnSpeedWire = wire(io).apply2(
	curry(function(turnLeft,turnRight) {
		if (turnLeft && turnRight) {
			return 0.0;
		} else if (turnLeft) {
			return -constants.turnSpeed;
		} else if (turnRight) {
			return constants.turnSpeed;
		} else {
			return 0.0;
		}
	}),
	keyDownWire(io, constants.turnLeft),
	keyDownWire(io, constants.turnRight)
);

var shipAngleWire = wire(io).o(
	wire(io).integral(0),
	shipTurnSpeedWire
);

var shipAccelerationWire = wire(io).apply2(
	curry(function(angle, accelerate) {
		if (accelerate) {
			return [
				-Math.sin(angle * Math.PI / 180.0) * constants.acceleration,
				Math.cos(angle * Math.PI / 180.0) * constants.acceleration
			];
		} else {
			return [0,0];
		}
	}),
	shipAngleWire,
	keyDownWire(io, constants.accelerate)
);

var shipVelocityWire = wire(io).o(
	wire(io).split(
		wire(io).integral(0),
		wire(io).integral(0)
	),
	shipAccelerationWire
);

var shipPositionWire = wire(io).o(
	wire(io).split(
		wire(io).integral(100),
		wire(io).integral(100)
	),
	shipVelocityWire
);

var shipFireWire = wire(io).o(
	wire(io).loopDelay(
		false,
		wire(io).o(
			wire(io).arr(function(x) {
				var fireKeyDown = x[0];
				var timeSinceFired = x[1];
				var r = fireKeyDown && (timeSinceFired >= 1.0 / constants.fireRate);
				return [r, r];
			}),
			wire(io).second(
				wire(io).o(
					wire(io).integralWith(
						curry(function(shipDidFire, timeSinceFired) {
							if (shipDidFire) {
								return 0.0;
							} else {
								return timeSinceFired;
							}
						}),
						0
					),
					wire(io).arr(function(x) {
						return [1.0, x];
					})
				)
			)
		)
	),
	keyDownWire(io, constants.fire)
);

var spawnBulletIO = function(pos, dir) {
	return function() {
		var bullet = document.createElementNS("http://www.w3.org/2000/svg", "circle");
		bullet.setAttribute("cx", pos[0]);
		bullet.setAttribute("cy", pos[1]);
		bullet.setAttribute("r", 0.5 * constants.bulletSize);
		svg.appendChild(bullet);
		commandWires.push(bulletCommandWire(bullet, pos, dir));
		return bullet;
	};
};

var bulletPositionWire = function(initPos, dir) {
	return wire(io).o(
		wire(io).split(
			wire(io).integral(initPos[0]),
			wire(io).integral(initPos[1])
		),
		wire(io).apply2(
			curry(function(a, b) {
				return [a[0]+b[0],a[1]+b[1]];
			}),
			wire(io).pure([
				dir[0] * constants.bulletSpeed,
				dir[1] * constants.bulletSpeed
			]),
			wire(io).o(
				wire(io).o(
					wire(io).hold,
					wire(io).now
				),
				shipVelocityWire
			)
		)
	);
};

var bulletCommandWire = function(bullet, initPos, dir) {
	return wire(io).next(
		wire(io).o(
			wire(io).for_(constants.bulletLifespan),
			wire(io).map(
				function(bulletPosition) {
					return {
						type:'sideEffect',
						value: function() {
							bullet.setAttribute("cx", bulletPosition[0]);
							bullet.setAttribute("cy", bulletPosition[1]);
						}
					};
				},
				bulletPositionWire(initPos, dir)
			)
		),
		function(dt, x) {
			return io.pure([
				wvChanged({
					type:'sideEffect',
					value: function() {
						svg.removeChild(bullet);
					}
				}),
				wire(io).inhibit
			]);
		}
	);
};

var spawnAsteroidIO = function(size) {
	return function() {
		var asteroid = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
		var asteroidConsts = constants.asteroids[size];
		var numVertices = randomInt(asteroidConsts.numVertices.min, asteroidConsts.numVertices.max+1);
		var vertex;
		var radius;
		var i;
		var angle;
		var verticesStr = "";
		var screenWidth = innerWidth;
		var screenHeight = innerHeight;
		for (i = 0; i < numVertices; ++i) {
			angle = i * 2.0 * Math.PI / numVertices;
			radius = randomFloat(asteroidConsts.radius.min, asteroidConsts.radius.max);
			vertex = [
				radius * Math.cos(angle),
				radius * Math.sin(angle)
			];
			if (i != 0) {
				verticesStr += " ";
			}
			verticesStr += vertex[0] + "," + vertex[1];
		}
		asteroid.setAttribute("fill", "none");
		asteroid.setAttribute("stroke", "white");
		asteroid.setAttribute("points", verticesStr);
		var initPos = [
			randomInt(0, screenWidth),
			randomInt(0, screenHeight)
		];
		asteroid.transform.baseVal
			.appendItem(svg.createSVGTransform())
			.setTranslate(initPos[0], initPos[1]);
		asteroid.transform.baseVal
			.appendItem(svg.createSVGTransform())
			.setRotate(0, 0, 0);
		svg.appendChild(asteroid);
		angle = randomFloat(0, 2.0 * Math.PI);
		var speed = randomFloat(asteroidConsts.speed.min, asteroidConsts.speed.max);
		var velocity = [
			speed * Math.cos(angle),
			speed * Math.sin(angle)
		];
		commandWires.push(asteroidCommandWire(asteroid, initPos, velocity));
		return asteroid;
	};
};

var screenSizeWire = function(dt, x) {
	var screenSizeWire2 = function(lastSize) {
		var unchangedChangedHandler = function(a) {
			return io.map(
				function(size) {
					if (size[0] == lastSize[0] && size[1] == lastSize[1]) {
						return [wvUnchanged(size), screenSizeWire2(size)];
					} else {
						return [wvChanged(size), screenSizeWire2(size)];
					}
				},
				function() {
					return [innerWidth, innerHeight];
				}
			);
		};
		return function(dt, x) {
			return x({
				wvInhibited: function() {
					return io.pure([wvInhibited, screenSizeWire2(lastSize)]);
				},
				wvUnchanged: unchangedChangedHandler,
				wvChanged: unchangedChangedHandler
			});
		};
	};
	var unchangedChangedHandler = function(a) {
		return io.map(
			function(size) {
				return [wvChanged(size), screenSizeWire2(size)];
			},
			function() {
				return [innerWidth, innerHeight];
			}
		);
	};
	return x({
		wvInhibited: function() {
			return io.pure([wvInhibited, screenSizeWire]);
		},
		wvUnchanged: unchangedChangedHandler,
		wvChanged: unchangedChangedHandler
	});
};

var screenWidthWire = wire(io).o(
	wire(io).arr(function(x) { return x[0]; }),
	screenSizeWire
);

var screenHeightWire = wire(io).o(
	wire(io).arr(function(x) { return x[1]; }),
	screenSizeWire
);

var asteroidPositionWire = function(initPos, vel) {
	return wire(io).o(
		wire(io).split(
			wire(io).o(
				wire(io).integralWith(
					curry(function(width, posX) {
						if (posX < 0) { return posX + width; }
						if (posX >= width) { return posX - width; }
						return posX;
					}),
					initPos[0]
				),
				wire(io).fanout(wire(io).id, screenWidthWire)
			),
			wire(io).o(
				wire(io).integralWith(
					curry(function(height, posY) {
						if (posY < 0) { return posY + height; }
						if (posY >= height) { return posY - height; }
						return posY;
					}),
					initPos[1]
				),
				wire(io).fanout(wire(io).id, screenHeightWire)
			)
		),
		wire(io).pure(vel)
	);
};

var asteroidCommandWire = function(asteroid, initPos, vel) {
	return wire(io).o(
		wire(io).arr(function(asteroidPos) {
			return {
				type:'sideEffect',
				value: function() {
					asteroid.transform.baseVal.getItem(0).setTranslate(asteroidPos[0], asteroidPos[1]);
				}
			};
		}),
		asteroidPositionWire(initPos, vel)
	);
};

var shipAngleCommandWire = wire(io).map(
	function(shipAngle) {
		return {type:'setShipAngle', value: shipAngle};
	},
	shipAngleWire
);

var shipPositionCommandWire = wire(io).map(
	function(shipPosition) {
		return {type:'setShipPosition', value: shipPosition};
	},
	shipPositionWire
);

var shipFireCommandWire = wire(io).map(
	function(fire) {
		if (fire) {
			return {type:'fire'};
		} else {
			return {type:'none'};
		}
	},
	shipFireWire
);

commandWires = [
	shipAngleCommandWire,
	shipPositionCommandWire,
	shipFireCommandWire
];

var lastMs = new Date().getTime();
var fireInput = function(input) {
	var ms = new Date().getTime();
	var dt = (ms - lastMs) / 1000.0;
	lastMs = ms;
	for (i = commandWires.length-1; i >= 0; --i) {
		var x = commandWires[i](dt, wvChanged(input))();
		commandWires[i] = x[1];
		x[0]({
			wvInhibited: function() {
				return function() {
					commandWires.splice(i, 1);
				};
			},
			wvUnchanged: function(a) {
				return function() {};
			},
			wvChanged: function(a) {
				return function() {
					if (a.type == 'setShipAngle') {
						shipDirection = [
							-Math.sin(a.value * Math.PI / 180.0),
							Math.cos(a.value * Math.PI / 180.0)
						];
						svgShipAngle.transform.baseVal.getItem(0).setRotate(a.value, 0, -8);
					} else if (a.type == 'setShipPosition') {
						shipPosition = a.value;
						svgShipPosition.transform.baseVal.getItem(0).setTranslate(a.value[0], a.value[1]);
					} else if (a.type == 'fire') {
						spawnBulletIO(shipPosition, shipDirection)();
						if (constants.enableSound) {
							play(audioCtx, pewBuffer)();
						}
						//fireInput(a);
					} else if (a.type == "sideEffect") {
						a.value();
					}
				};
			}
		})();
	}
};

var init = function() {
	svg = document.getElementById("svg");
	svgShipAngle = document.getElementById("svgShipAngle");
	svgShipPosition = document.getElementById("svgShipPosition");

	if (constants.enableSound) {
		explosionBuffer = render(audioCtx, explosion(wire(io), io))();
		ufoBuffer = render(audioCtx, ufo(wire(io), io))();
		deathBuffer = render(audioCtx, death(wire(io), io))();
		pewBuffer = render(audioCtx, pew(wire(io), io))();
	}

	for (var i = 0; i < 5; ++i) {
		spawnAsteroidIO(3)();
	}

	var lastMs = new Date().getTime();
	setInterval(
		function() {
			var ms = new Date().getTime();
			var dt = (ms - lastMs) / 1000.0;
			fireInput({type:'dt', dt: dt});
			lastMs = ms;
		},
		16
	);
};

document.onkeydown = function(e) {
	fireInput({type:"keyDown", keyCode: e.keyCode});
	if (constants.enableSound) {
		if (e.keyCode == 49) {
			play(audioCtx, explosionBuffer)();
		} else if (e.keyCode == 50) {
			play(audioCtx, ufoBuffer)();
		} else if (e.keyCode == 51) {
			play(audioCtx, deathBuffer)();
		} else if (e.keyCode == 52) {
			play(audioCtx, pewBuffer)();
		}
	}
};

document.onkeyup = function(e) {
	fireInput({type:"keyUp", keyCode: e.keyCode});
};