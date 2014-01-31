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

	return {
		checkUserObject: checkUserObject
	};
}]);