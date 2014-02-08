sane.services.factory('stateService', ['$state', function ($state) {
	var stateHistory = [];

	function getCurrentState() {
		return $state.current.name;
	}

	function getPreviousState() {
		return stateHistory.slice(0, 1);
	}

	function goBack() {
		var previousState = stateHistory.shift();
		previousState && $state.go(previousState);
	}

	function from(state) {
		if (state && state !== stateHistory[0]) stateHistory.unshift(state);
	}

	function goTo(state) {
		$state.go(state);

		return {
			from: from
		};
	}

	return {
		getCurrentState: getCurrentState,
		getPreviousState: getPreviousState,
		goTo: goTo,
		goBack: goBack
	};
}]);
