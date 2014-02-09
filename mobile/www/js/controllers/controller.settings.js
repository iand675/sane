sane.controllers.controller('settingsController', [
	'$scope',
	'userStorageService',
function ($scope, userStorageService) {
	$scope.user = userStorageService.getUserObject().data;
}]);
