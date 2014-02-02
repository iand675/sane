sane.controller('viewController', [
	'$scope', 
function ($scope) {
	$scope.$on('$stateChangeStart', function (event, toState) {
		if (toState.name === 'welcome') {
			$scope.animation = 'softAnimation'; 
		} else {
			$scope.animation = 'hardAnimation'; 
		}
		
		if (!$scope.$$phase)
			$scope.$apply();
    });
}]);
