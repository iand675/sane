sane.controller('loginController', [
	'$scope', 
	'authenticationService', 
	'stateService', 
function ($scope, authenticationService, stateService) {
	$scope.login = function () {
		if ($scope.loginForm.$invalid)
			return false;

		authenticationService.authenticateEmailStrategy($scope.user).then(function () {
			stateService.setStateHome();
		}, function () {
			$scope.serverError = true;
		});
	};
}]);