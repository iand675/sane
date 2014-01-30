sane.factory('preflightControllerService', [function () {
	this.checkUserObject = function () {
		saneUser = localStorageService.get('saneUser');

		if (saneUser) 
			auth.authenticate(saneUser).then(function () {
				$state.go('home');
			}, function () {
				$state.go('welcome');
			});
		else 
			checkConnectionStatus().then(function () {
				$state.go('welcome');
			}, function () {
				$state.go('noConnection');
			});
	};

	function checkAuthenticationForLogin() {
		authenticationService.authenticate.then(setStateHome, setStateWelcome);
	}

	function checkConnectionForLogin() {
		connectionService.checkConnectionStatus.then(setStateWelcome, setStateNoConnection);
	}

	return {
		checkAuthenticationForLogin: checkAuthenticationForLogin,
		checkConnectionForLogin: checkConnectionForLogin
	};
}]);