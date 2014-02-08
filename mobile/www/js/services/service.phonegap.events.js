sane.services.factory('phonegapEventsService', ['$q', function ($q) {
	
	document.addEventListener('pause', function () {
			console.log("Paused!");
		}, false);

	document.addEventListener('startcallbutton', function () {
			console.log("Paused!");
		}, false);

	document.addEventListener('endcallbutton', function () {
			console.log("Paused!");
		}, false);

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
