sane.factory('authenticationService', ['facebookService', function (fb) {

	function checkEmailAuth(user, cb) {

	}

	function checkFacebookAuth(user, cb) {
		fb.checkAuthStatus({
			success: cb.success(),
			failure: cb.failure(),
			noConnection: cb.noConnection()
		});
	}

	function checkAuthType(user, cb) {
		switch(user.authType) {
			case 'facebook': 
				checkFacebookAuth(user, cb);
				break;
			case 'email':
				checkEmailAuth(user, cb);
				break;
			default:
				cb.failure();
		}
	}

	function authenticate(user, cb) {
		if (!user.authType)
			cb.failure();
		else
			checkAuthType(user, cb);
	} 

	return {
		authenticate: authenticate
	};
}]);