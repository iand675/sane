sane.services.factory('userStorageService', [
	'$q', 
	'localStorageService', 
function ($q, localStorageService) {
	function checkUserObject() {
		var deferred = $q.defer(),
			saneUser = localStorageService.getItem('saneUser');

		saneUser ? deferred.resolve(saneUser) : deferred.reject();

		return deferred.promise;
	}

	function createFacebookUserObject(authResponse, userObject) {
		facebookUserObject = {
			authType: 'facebook',
			credentials: authResponse,
			data: userObject
		};

		localStorageService.setItem('saneUser', facebookUserObject);
	}

	function createStandardUserObject(credentials, userObject) {
		standardUserObject = {
			authType: 'email',
			credentials: credentials,
			data: userObject
		};

		localStorageService.setItem('saneUser', standardUserObject);
	}

	function deleteUserObject() {
		localStorageService.remove('saneUser');
	}

	function getUserObject() {
		return localStorageService.getItem('saneUser');
	}

	return {
		checkUserObject: checkUserObject,
		createFacebookUserObject: createFacebookUserObject,
		createStandardUserObject: createStandardUserObject,
		getUserObject: getUserObject,
		deleteUserObject: deleteUserObject
	};
}]);
