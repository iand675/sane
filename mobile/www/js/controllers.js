sane
	.controller('mainController', ['$state', function ($state) {
		$state.go('welcome');
	}])
	.controller('welcomeController', ['$state', '$scope', function ($state, $scope) {
		$scope.backgroundImageUrl = "{'background-image': 'url(img/background/" + Math.floor((Math.random() * 10) + 1)  + ".jpg)'}";
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