sane.controller('preflightController', [
	'$scope',
	'$state',
	'phonegapEventsService',
	'userStorageService',
	'preflightControllerService',
function ($scope, $state, phonegapEventsService, userStorageService, preflightControllerService) {
	
	$scope.onUserObjectFound = function () {
		preflightControllerService.checkAuthenticationForLogin();
	};

	$scope.onUserObjectNotFound = function () {
		preflightControllerService.checkConnectionForLogin();
	};

	$scope.onDeviceReady = function () {
		// userStorageService.checkUserObject().then(function () {
		// 	$scope.onUserObjectFound();
		// }, function () {
		//	$scope.onUserObjectNotFound();
		// });
	};

	phonegapEventsService.onDeviceReady().then(function () {
		$scope.onDeviceReady();
		$state.go('welcome');
	});

}]);