sane.factory('phonegapEventsService', ['$q', function ($q) {
	
	function onDeviceReady() {
		var deferred = $q.defer();

		document.addEventListener('deviceready', function () {
			deferred.resolve();
		}, false);

		return deferred.promise;
	}

	return {
		onDeviceReady: onDeviceReady
	};
}]);