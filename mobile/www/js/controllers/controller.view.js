sane.controllers.controller('viewController', [
	'$scope',
	'stateService',
	'animationService',
function ($scope, stateService, animationService) {
	$scope.animation = 'soft-enter soft-leave';

	$scope.$apply();

	$scope.state = {
		goTo: function (state) {
			$scope.animation = animationService.setForwardsAnimation(state);
			return stateService.goTo(state);
		},
		goBack: function() {
			$scope.animation = animationService.setBackwardsAnimation();
			stateService.goBack();
		}
	};
}]);
