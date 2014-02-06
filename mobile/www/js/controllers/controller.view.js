sane.controller('viewController', [
	'$scope',
	'stateService',
function ($scope, stateService) {
	$scope.animation = {
		soft: true
	};

	$scope.state = {
		goTo: function (state) {
			if (state === 'welcome') {
				$scope.animation = {
					soft: true
				};
			}
			else {
				$scope.animation = {
					forwards: true
				};
			}

			return stateService.goTo(state);
		},
		goBack: function() {
			$scope.animation = {
				backwards: true
			};
			$scope.$apply();
			stateService.goBack();
		}
	};
}]);
