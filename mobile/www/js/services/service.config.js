sane.services.factory('configService', [function () {	
	var baseUri = 'http://162.242.229.57:3000',
		server = {
			baseUri: baseUri,
			pingUri: baseUri + '/ping',
			signinUri: baseUri + '/signin',
			usersUri: baseUri + '/users'
		},
		animationOverrides = {
			welcome: 'soft',
			home: 'soft'
		};

	return {
		server: server,
		animationOverrides: animationOverrides
	};
}]);
