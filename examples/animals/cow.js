
let cow = animal.subclass((that, my) => {
	that.alert = function() {
		return "moo";
	};
});
