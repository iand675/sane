sane.controller('preflightController', [
	'$scope',
	'phonegapEventsService',
	'userStorageService',
	'facebookService',
	'authenticationService',
	'connectionService',
	'stateService',
function ($scope, phonegapEventsService, userStorageService, facebookService, authenticationService, connectionService, stateService) {
	
	$scope.initializeForUnknownUser = function () {
		connectionService.checkConnectionToServer().then(function () {
			stateService.setStateWelcome();
		}, function () {
			stateService.setStateNoConnection();
		});
	};

	$scope.onDeviceReady = function () {
		authenticationService.authenticateNoStrategy().then(function () {
			stateService.setStateHome();
		}, function () {
			$scope.initializeForUnknownUser();
		});
	};

	phonegapEventsService.onDeviceReady().then(function () {
		facebookService.initialize();
		$scope.onDeviceReady();
	});
}]);
