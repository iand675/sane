sane.factory('stateService', ['$state', function ($state) {
	var stateHistory = [];

	function goBack() {
		var previousState = stateHistory.shift();

		previousState && $state.go(previousState);
	}

	function from(state) {
		if (state && state !== stateHistory[0]) stateHistory.push(state);
	}

	function goTo(state) {
		$state.go(state);

		return {
			from: from
		};
	}

	return {
		goTo: goTo,
		goBack: goBack
	};
}]);
