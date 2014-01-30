sane.factory('connectionService', [function () {

	function checkConnectonToServer() {
		var deferred = $q.defer();

		$http({method: 'GET', url: 'https://saneapp.com'})
			.success(function (response) {
				deferred.resolve();
			})
			.error(function (response) {
				if (response.status == 503 || !response.status)
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