sane.services.factory('connectionService', [
	'$http', 
	'$q',
	'config',
function ($http, $q, config) {

	function checkConnectionToServer() {
		var deferred = $q.defer();

		$http({
				method: 'GET', 
				url: config.server.pingUri, 
				timeout: 4000
			}).success(function (data, status, headers, config) {
				deferred.resolve();
			}).error(function (data, status, headers, config) {
				if (status === 503 || status === 0)
					deferred.reject();
				else
					deferred.resolve();
			});

		return deferred.promise;
	}

	return {
		checkConnectionToServer: checkConnectionToServer
	};

}]);
