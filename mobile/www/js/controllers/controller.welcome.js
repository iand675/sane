sane.controllers.controller('welcomeController', [
	'$scope', 
	'authenticationService',
	'stateService', 
function ($scope, authenticationService, stateService) {
	$scope.backgroundImageUrl = "{'background-image': 'url(img/background/" + Math.floor((Math.random() * 10) + 1)  + ".jpg)'}";

	$scope.fbLogin = function () {
		authenticationService.authenticateFacebookStrategy().then(function () {
			$scope.state.goTo('home');
		});
	};
}]);
