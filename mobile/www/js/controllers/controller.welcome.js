sane.controller('welcomeController', ['$scope', '$state', 'facebookService', function ($scope, $state, facebookService) {
	$scope.backgroundImageUrl = "{'background-image': 'url(img/background/" + Math.floor((Math.random() * 10) + 1)  + ".jpg)'}";

	$scope.fbLogin = function () {
		facebookService.login();
	};

	$scope.fbLogout = function () {
		console.log("Attempting logout...");
		FB.logout(function (response) {
			console.log(response);
		});
	};
}]);