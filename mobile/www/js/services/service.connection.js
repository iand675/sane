sane.factory('connectionService', ['$http', '$q', function ($http, $q) {

	function checkConnectionToServer() {
		var deferred = $q.defer();

		$http({method: 'GET', url: 'https://saneapp.com', timeout: 4000})
			.success(function (response) {
				deferred.resolve();
			})
			.error(function (response) {
				response = response || {};

				if (!response.status || response.status == 503)
					deferred.reject();
				else
					deferred.resolve();
			});

		return deferred.promise;
	}

	function getConnectionType() {
	}

	return {
		checkConnectionToServer: checkConnectionToServer,
		getConnectionType: getConnectionType
	};

}]);