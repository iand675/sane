var sane = angular.module('sane', [
	'ui.router',
	'ngAnimate'
]).config(function($stateProvider, $urlRouterProvider) {
	$stateProvider
		.state('welcome', {
			url: '/',
			templateUrl: 'templates/welcome.html',
			controller: 'welcomeController'
		})
		.state('login', {
			url: '/login',
			templateUrl: 'templates/login.html',
			controller: 'loginController'
		})
		.state('signup', {
			url: '/signup',
			templateUrl: 'templates/signup.html',
			controller: 'signupController'
		})
		.state('home', {
			url: '/home',
			templateUrl: 'templates/home.html'
		})
		.state('noConnection', {
			url: '/noconnection',
			templateUrl: 'templates/noconnection.html'
		});
});
