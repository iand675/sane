var sane = angular.module('sane', [
	'ui.router',
	'ngAnimate',
	'sane.services',
	'sane.controllers',
	'sane.directives',
	'sane.routing',
	'sane.config'
]).run(['phonegapEventsService', 'userStorageService', 'facebookService', 'authenticationService', 'connectionService', 'stateService', function(phonegapEventsService, userStorageService, facebookService, authenticationService, connectionService, stateService) {
	FastClick.attach(document.body);

	function initializeForUnknownUser() {
		connectionService.checkConnectionToServer().then(function () {
			stateService.goTo('welcome');
		}, function () {
			stateService.goTo('noconnection');
		});
	}

	function onDeviceReady() {
		authenticationService.authenticateNoStrategy().then(function () {
			stateService.goTo('home');
		}, function () {
			initializeForUnknownUser();
		});
	}

	phonegapEventsService.onDeviceReady().then(function () {
		facebookService.initialize();
		onDeviceReady();
	});
}]);
