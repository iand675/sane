sane.factory('facebook', function () {
	
	function updateAuthStatus(authResponse) {
		var user;

		if (localStorage.saneUser) {
			user = JSON.parse(localStorage.saneUser);
			user.fb = JSON.stringify(response.authResponse);
			localStorage.saneUser = JSON.stringify(user);
		}
	}

	function getAuthStatus(cb) {
		FB.getLoginStatus(function (response) {
			if (response.status === 'connected') {
				updateAuthStatus(response.authResponse)
				cb.success();
			}
			else {
				cb.failure();
			}
		});
	}

	function init(cb) {
		window.fbAsyncInit = function() {
			cb();
		};

		FB.init({
			appId: '770257949654604',
			nativeInterface: CDV.FB,
			useCachedDialogs: false
		});
	}

	function checkAuthStatus(cb) {
		if (FB && FB.getLoginStatus) {
			getAuthStatus(cb);
		}
		else {
			init(function() {
				getAuthStatus(cb);
			});
		}
	}

	return {
		init: init,
		checkAuthStatus: checkAuthStatus
	};
});

sane.factory('authentication', ['facebook', function (fb) {

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