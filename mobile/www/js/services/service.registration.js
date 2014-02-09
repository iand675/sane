sane.services.factory('registrationService', [
	'$q', 
	'$http',
	'facebookService', 
	'userStorageService',
	'config',
function ($q, $http, facebookService, userStorageService, configService) {
	function createStandardUser(user) {
		var deferred = $q.defer();

		user.type = 'standard';
		user.username = user.username.toLowerCase();

		$http({
			method: 'POST', 
			url: config.server.usersUri,
			data: user,
			timeout: 4000
		}).success(function (userObject, status, headers, config) {
			userObject.password = user.password;
			userStorageService.createStandardUserObject(userObject);
			deferred.resolve();
		}).error(function (data, status, headers, config) {
			deferred.reject();
		});

		return deferred.promise;
	}

	return {
		createStandardUser: createStandardUser
	};
}]);
