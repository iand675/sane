sane.factory('connectionService', [
	'$http', 
	'$q', 
	'configService', 
function ($http, $q, configService) {

	function checkConnectionToServer() {
		var deferred = $q.defer();

		$http({
				method: 'GET', 
				url: configService.server.pingUri, 
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
