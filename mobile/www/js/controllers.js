sane
	.controller('mainController', ['$state', function ($state) {
		$state.go('welcome');
	}])
	.controller('welcomeController', ['$state', '$scope', function ($state, $scope) {
		$scope.backgroundImageUrl = "{'background-image': 'url(img/background/" + Math.floor((Math.random() * 5) + 1)  + ".jpg)'}";
	}]);