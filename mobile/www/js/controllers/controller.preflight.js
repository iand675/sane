sane.controller('preflightController', [
	'$scope',
	'phonegapEventsService',
	'userStorageService',
	'authenticationService',
	'connectionService',
	'stateService',
function ($scope, phonegapEventsService, userStorageService, authenticationService, connectionService, stateService) {
	
	$scope.onUserObjectFound = function () {
		authenticationService.authenticate.then(function () {
			stateService.setStateHome();
		}, function () {
			stateService.setStateNoConnection();
		});
	};

	$scope.onUserObjectNotFound = function () {
		connectionService.checkConnectionToServer().then(function () {
			stateService.setStateWelcome();
		}, function () {
			stateService.setStateNoConnection();
		});
	};

	$scope.onDeviceReady = function () {
		userStorageService.checkUserObject().then(function () {
			$scope.onUserObjectFound();
		}, function () {
			$scope.onUserObjectNotFound();
		});
	};

	phonegapEventsService.onDeviceReady().then(function () {
		$scope.onDeviceReady();
	});

}]);