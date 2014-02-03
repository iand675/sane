sane.directive('uiMenu', ['authenticationService', 'stateService', function (authenticationService, stateService) {
	return {
		restrict: 'E',
		scope: true,
		templateUrl: 'templates/directives/ui-menu.html',
		link: function (scope, element, attrs) {
			scope.iconClicked = false;

			function preventRubberBandScroll(e) {
				e.preventDefault(); 
			}

			scope.clicked = function () {
				document.addEventListener('touchmove', preventRubberBandScroll);
				scope.iconClicked = true;
			};

			scope.hideMenu = function () {
				document.removeEventListener('touchmove', preventRubberBandScroll);
				scope.iconClicked = false;
			};

			scope.logout = function () {
				authenticationService.logout().then(function () {
					document.removeEventListener('touchmove', preventRubberBandScroll);
					scope.iconClicked = false;
					stateService.setStateWelcome();
				});
			};
		}
	};
}]);