sane.factory('userStorageService', [
	'$q', 
	'localStorageService', 
function ($q, localStorageService) {
	function checkUserObject() {
		var deferred = $q.defer(),
			saneUser = localStorageService.getItem('saneUser');

		saneUser ? deferred.resolve(saneUser) : deferred.reject();

		return deferred.promise;
	}

	function createFacebookUserObject(facebookResponse) {
		facebookUserObject = {
			authType: 'facebook',
			facebook: facebookResponse
		};

		localStorageService.setItem('saneUser', facebookUserObject);
	}

	function createStandardUserObject(standardResponse) {
		standardUserObject = {
			authType: 'email',
			standard: standardResponse
		};

		localStorageService.setItem('saneUser', standardUserObject);
	}

	return {
		checkUserObject: checkUserObject,
		createFacebookUserObject: createFacebookUserObject,
		createStandardUserObject: createStandardUserObject
	};
}]);
