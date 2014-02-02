sane.factory('authenticationService', [
	'$q', 
	'$http', 
	'facebookService', 
	'userStorageService',
	'configService',
function ($q, $http, facebookService, userStorageService, configService) {

	function checkUserCookie() {
		var deferred = $q.defer();

		deferred.resolve();

		return deferred.promise;
	}

	function authenticateEmailStrategy(credentials) {
		var deferred = $q.defer();

		credentials.type = 'standard';
		credentials.username = user.username.toLowerCase();

		$http({
				method: 'POST', 
				url: configService.server.signinUri,
				data: credentials,
				timeout: 4000
			}).success(function (userObject, status, headers, config) {
				userObject.password = credentials.password;
				userStorageService.createStandardUserObject(userObject);
				deferred.resolve();
			}).error(function (data, status, headers, config) {
				deferred.reject();
			});

		return deferred.promise;
	}

	function authenticateFacebookStrategy() {
		var deferred = $q.defer();

		facebookService.login().then(function (authResponse) {
			authResponse.type = "facebook";

			$http({
					method: 'POST', 
					url: configService.server.signinUri,
					data: authResponse,
					timeout: 4000
				}).success(function (userObject, status, headers, config) {
					deferred.resolve();
				}).error(function (data, status, headers, config) {
					deferred.resolve();
				});
		}, function () {
			deferred.reject();
		});

		return deferred.promise;
	}

	function authenticateNoStrategy() {
		var deferred = $q.defer();

		userStorageService.checkUserObject().then(function (userObject) {
			var authType = userObject.authType;

			if (authType === 'facebook') {
				facebookService.isUserAuthenticated().then(function () { 
					deferred.resolve(); 
				}, function() { 
					deferred.reject(); 
				});
			}
			else if (authType === 'email') {
				checkUserCookie().then(function () { 
					deferred.resolve(); 
				}, function() { 
					deferred.reject(); 
				});
			}
			else {
				deferred.reject();
			}
		}, function () {
			facebookService.isUserAuthenticated().then(function () { 
				deferred.resolve(); 
			}, function() { 
				deferred.reject(); 
			});
		});

		return deferred.promise;
	} 

	return {
		authenticateNoStrategy: authenticateNoStrategy,
		authenticateFacebookStrategy: authenticateFacebookStrategy,
		authenticateEmailStrategy: authenticateEmailStrategy
	};
}]);
