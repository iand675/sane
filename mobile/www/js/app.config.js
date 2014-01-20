sane.config(function($stateProvider, $urlRouterProvider) {
	$stateProvider
		.state('welcome', {
			url: '/',
			templateUrl: 'templates/welcome.html',
			controller: 'welcomeController'
		});
});