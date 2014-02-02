sane.controller('signupController', [
	'$scope', 
	'registrationService', 
	'stateService', 
function ($scope, registrationService, stateService) {

	$scope.createUser = function () {
		if ($scope.signupForm.$invalid)
			return false;

		registrationService.createStandardUser($scope.user).then(function () {
			stateService.setStateHome();
		}, function () {
			$scope.serverError = true;
		});
	};
}]);
