sane.factory('configService', [function () {	
	var baseUri = 'http://162.242.229.57:3000';

	return {
		server: {
			baseUri: baseUri,
			pingUri: baseUri + '/ping',
			signinUri: baseUri + '/signin',
			usersUri: baseUri + '/users'
		}
	};
}]);
