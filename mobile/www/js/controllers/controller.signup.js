sane.controllers.controller('signupController', [
	'$scope', 
	'registrationService', 
	'stateService', 
function ($scope, registrationService, stateService) {

	$scope.createUser = function () {
		if ($scope.signupForm.$invalid)
			return false;

		registrationService.createStandardUser($scope.user).then(function () {
			stateService.goTo('home');
		}, function () {
			$scope.serverError = true;
		});
	};
}]);
