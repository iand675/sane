sane.factory('authenticationService', [
	'$q', 
	'facebookService', 
	'userStorageService', 
	'stateService',
function ($q, facebookService, userStorageService, stateService) {

	function checkUserCookie() {

	}

	function authenticate(user, cb) {
		var deferred = $q;

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
			deferred.reject();
		});

		return deferred.promise;
	} 

	return {
		authenticate: authenticate
	};
}]);