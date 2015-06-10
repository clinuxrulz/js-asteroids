// randomInt: returns a random number between min (inclusive) and max (exclusive)
var randomInt = function(min, max) {
	return Math.floor(Math.random() * (max - min)) + min;
};

var randomFloat = function(min, max) {
	return Math.random() * (max - min) + min;
}