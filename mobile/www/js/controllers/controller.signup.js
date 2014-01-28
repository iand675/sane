sane.controller('signupController', ['$scope', '$state', '$http', function ($scope, $state, $http) {
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