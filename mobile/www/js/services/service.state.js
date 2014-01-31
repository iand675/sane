sane.factory('stateService', ['$state', function ($state) {

	return {
		setStateHome: function () {
			$state.go('home');
		},
		setStateWelcome: function () {
			$state.go('welcome');
		},
		setStateNoConnection: function () {
			$state.go('welcome');
		},
	};

}]);