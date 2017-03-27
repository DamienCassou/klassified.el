
let bird = animal.abstractSubclass((that, my) => {
	that.alert = function() {
		return "tweet";
	};
});
