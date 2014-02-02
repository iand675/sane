sane.factory('facebookService', ['$q', 'userStorageService', function ($q, userStorageService) {
	
	function isUserAuthenticated() {
		var deferred = $q.defer();

		FB.getLoginStatus(function (response) {
			if (response.status === 'connected') {
				userStorageService.createFacebookUserObject(response);
				deferred.resolve();
			}				
			else {
				deferred.reject();
			}
		});

		return deferred.promise;
	}

	function login() {
		var deferred = $q.defer();
		FB.login(function (response) {
			if (response.authResponse) {
				userStorageService.createFacebookUserObject(response);
				deferred.resolve();
			}
			else {
				deferred.reject();
			}
		}, {scope: 'email'});

		return deferred.promise;
	}

	function initialize() {
		FB.init({
			appId: '770257949654604',
			nativeInterface: CDV.FB,
			useCachedDialogs: false
		});
	}

	return {
		login: login,
		isUserAuthenticated: isUserAuthenticated,
		initialize: initialize
	};
}]);
