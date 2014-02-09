sane.routing = angular.module('sane.routing', [
	'ui.router',
	'ngAnimate'
]).config(function($stateProvider, $urlRouterProvider) {
	function preventRubberBandScroll(e) {
		e.preventDefault(); 
	}

	$stateProvider
		.state('welcome', {
			url: '/',
			templateUrl: 'templates/welcome.html',
			controller: 'welcomeController',
			onEnter: function () {
				document.addEventListener('touchmove', preventRubberBandScroll);
			},
			onExit: function () {
				document.removeEventListener('touchmove', preventRubberBandScroll);	
			}
		})
		.state('login', {
			url: '/login',
			templateUrl: 'templates/login.html',
			controller: 'loginController',
			onEnter: function () {
				document.addEventListener('touchmove', preventRubberBandScroll);
			},
			onExit: function () {
				document.removeEventListener('touchmove', preventRubberBandScroll);	
			}
		})
		.state('forgotpassword', {
			url: '/forgotpassword',
			templateUrl: 'templates/forgotpassword.html',
			controller: 'forgotPasswordController',
			onEnter: function () {
				document.addEventListener('touchmove', preventRubberBandScroll);
			},
			onExit: function () {
				document.removeEventListener('touchmove', preventRubberBandScroll);	
			}
		})
		.state('signup', {
			url: '/signup',
			templateUrl: 'templates/signup.html',
			controller: 'signupController',
			onEnter: function () {
				document.addEventListener('touchmove', preventRubberBandScroll);
			},
			onExit: function () {
				document.removeEventListener('touchmove', preventRubberBandScroll);	
			}
		})
		.state('settings', {
			url: '/settings',
			templateUrl: 'templates/settings.html',
			controller: 'settingsController'
		})
		.state('home', {
			url: '/home',
			templateUrl: 'templates/home.html'
		})
		.state('noconnection', {
			url: '/noconnection',
			templateUrl: 'templates/noconnection.html'
		});
});