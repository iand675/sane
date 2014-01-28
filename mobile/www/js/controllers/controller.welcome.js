sane.controller('welcomeController', ['$scope', '$state', function ($scope, $state) {
	$scope.backgroundImageUrl = "{'background-image': 'url(img/background/" + Math.floor((Math.random() * 10) + 1)  + ".jpg)'}";

	$scope.fbAuth = function () {
		FB.init({
			appId: '770257949654604',
			nativeInterface: CDV.FB,
			useCachedDialogs: false
		});

		FB.Event.subscribe('auth.statusChange', function (session) {
			console.log(session);
		});

		FB.getLoginStatus(function (session) {
			console.log(session);
		});
	};

	$scope.fbLogin = function () {
		FB.login(null, {scope: 'email'});
	};

	$scope.fbLogout = function () {
		console.log("Attempting logout...");
		FB.logout(function (response) {
			console.log(response);
		});
	};
}]);