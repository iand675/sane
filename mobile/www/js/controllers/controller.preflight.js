sane.controller('preflightController', ['$scope', '$state', '$http', '$q', 'authenticationService', function ($scope, $state, $http, $q, auth) {

	$scope.checkConnectionStatus = function () {
		var deferred = $q.defer();

		$http({method: 'GET', url: 'https://saneapp.com'})
			.success(function (response) {
				deferred.resolve();
			})
			.error(function (response) {
				if (response.status == 503 || !response.status)
					deferred.resolve();
				else
					deferred.reject();
			});

		return deferred.promise;
	};

	$scope.checkUserObject = function () {
		if (localStorage.saneUser) 
			auth.authenticate(JSON.parse(localStorage.saneUser), {
				success: function () {
					$state.go('home');
				},
				failure: function () {
					$state.go('welcome');
				},
				noConnection: function () {
					$state.go('noConnection');
				}
			});
		else 
			checkConnectionStatus().then(function () {
				$state.go('welcome');
			}, function () {
				$state.go('noConnection');
			});
	};

	document.addEventListener('deviceready', function () {
		$scope.checkUserObject();
	}, false);
}]);