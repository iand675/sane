sane.config = angular.module('sane.config', [])
	.value('config', {
		server : {
			baseUri: 'http://162.242.229.57:3000',
			pingUri: 'http://162.242.229.57:3000/ping',
			signinUri: 'http://162.242.229.57:3000/signin',
			usersUri: 'http://162.242.229.57:3000/users'
		},
		animationOverrides: {
			welcome: 'soft',
			home: 'soft'
		}
	});