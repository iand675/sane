sane
	.controller('preflightController', ['$state', 'authentication', function ($state, auth) {
		function checkConnectionStatus() {
			return navigator.onLine;
		}

		function  checkUserObject() {
			if (localStorage.saneUser) 
				auth.authenticate(JSON.parse(saneUser), {
					success: function () {
						$state.go('home');
					},
					failure: function () {
						$state.go('welcome');
					},
					noConnection: function () {
						$state.go('noConnection');
					}
				});
			else 
				checkConnectionStatus() ? $state.go('welcome') : $state.go('noConnection');
		}

		document.addEventListener('deviceready', function () {
			checkUserObject();
		}, false);
	}])
	.controller('welcomeController', ['$state', '$scope', function ($state, $scope) {
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
	}])
	.controller('signupController', ['$state', '$scope', '$http', function ($state, $scope, $http) {
		$scope.name = '';
		$scope.email = '';
		$scope.username = '';
		$scope.password = '';

		$scope.createUser = function () {
			$http.post('http://192.168.33.10:3000/users', {
				name: $scope.name,
				email: $scope.email,
				username: $scope.username,
				password: $scope.password
			}).success(function () {
				console.log("Success");
			}).error(function (err) {
				console.log(err);
			});
		};
	}]);