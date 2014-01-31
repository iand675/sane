sane.factory('connectionService', ['$http', '$q', function ($http, $q) {

	function checkConnectionToServer() {
		var deferred = $q.defer();

		$http({method: 'GET', url: 'https://saneapp.com/ping', timeout: 4000})
			.success(function (data, status, headers, config) {
				deferred.resolve();
			})
			.error(function (data, status, headers, config) {
				if (status == 503 || status == 0)
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