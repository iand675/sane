sane.controllers.controller('forgotPasswordController', [
	'$scope',
	'authenticationService',
function ($scope, authenticationService) {
	$scope.recoverPassword = function () {
		if ($scope.forgotPasswordForm.$invalid)
			return false;

		authenticationService.recoverPassword($scope.user.email).then(function () {
			$scope.recoverySuccess = true;
		}, function () {
			$scope.serverError = true;
		});
	};
}]);
