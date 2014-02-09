sane.services.factory('facebookService', ['$q', 'userStorageService', function ($q, userStorageService) {
	
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

	function getUserDetails() {
		var deferred = $q.defer();

		FB.api('/me', 
		{
			fields: "picture.type(large), email, name, username"
		}, 
		function (userObject) {
			if (!userObject || userObject.error) {
				deferred.resolve(userObject.error);
			}
			else {
				userObject.image = userObject.picture.data.url;
				deferred.resolve(userObject);
			}
		});

		return deferred.promise;
	}

	function login() {
		var deferred = $q.defer();
		FB.login(function (response) {
			if (response.authResponse) {
				deferred.resolve(response.authResponse);
			}
			else {
				deferred.reject();
			}
		}, {scope: 'email'});

		return deferred.promise;
	}

	function logout() {
		var deferred = $q.defer();

		FB.logout(function () {});

		deferred.resolve();

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
		logout: logout,
		isUserAuthenticated: isUserAuthenticated,
		getUserDetails: getUserDetails,
		initialize: initialize
	};
}]);
