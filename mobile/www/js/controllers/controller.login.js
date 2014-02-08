sane.controllers.controller('loginController', [
	'$scope', 
	'authenticationService',
function ($scope, authenticationService) {
	$scope.login = function () {
		if ($scope.loginForm.$invalid)
			return false;

		authenticationService.authenticateEmailStrategy($scope.user).then(function () {
			$scope.state.goTo('home');
		}, function () {
			$scope.serverError = true;
		});
	};
}]);
