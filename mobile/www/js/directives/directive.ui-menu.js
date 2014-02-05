sane.directive('uiMenu', ['$timeout', 'authenticationService', 'stateService', function ($timeout, authenticationService, stateService) {
	var directiveDefinitionObject = {
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

					$timeout(function () {
						scope.iconClicked = false;
						stateService.setStateWelcome();
					}, 250);
				});
			};
		}
	};

	return directiveDefinitionObject;
}]);