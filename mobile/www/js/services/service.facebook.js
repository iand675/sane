sane.factory('facebookService', ['$q', function ($q) {
	
	function updateLocalAuthStatus(authResponse) {
		var user;

		if (localStorage.saneUser) {
			user = JSON.parse(localStorage.saneUser);
			user.fb = JSON.stringify(response.authResponse);
			localStorage.saneUser = JSON.stringify(user);
		}
	}

	function getAuthStatus() {
		var deferred = $q.defer();

		FB.getLoginStatus(function (response) {
			deferred.resolve(response);
		});

		return deferred.promise;
	}

	function init() {
		var deferred = $q.defer();

		window.fbAsyncInit = function() {
			deferred.resolve();
		};

		FB.init({
			appId: '770257949654604',
			nativeInterface: CDV.FB,
			useCachedDialogs: false
		});

		return deferred.promise;
	}

	function checkAuthStatus(cb) {
		var authStatus, initStatus;

		if (FB && FB.getLoginStatus) {	
			authStatus = getAuthStatus();
			authStatus.then(function (response) {
				resolveAuthStatus(response);
			});
		}
		else {
			initStatus = init();

			initStatus.then(function () {
				authStatus = getAuthStatus();

				authStatus.then(function (response) {
					resolveAuthStatus(response);
				});
			});
		}

		function resolveAuthStatus(response) {
			if (response.status === 'connected') {
				updateLocalAuthStatus(response.authResponse);
				cb.success();
			}
			else {
				cb.failure();
			}
		}
	}

	return {
		init: init,
		checkAuthStatus: checkAuthStatus
	};
}]);