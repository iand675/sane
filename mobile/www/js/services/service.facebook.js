sane.factory('facebookService', [function () {
	var isFacebookInitialized = !!window.FB && !!window.getLoginStatus;
	
	function isUserAuthenticated() {
		var deferred = $q.defer();

		checkInitialization().then(function () {
			FB.getLoginStatus(function (response) {
				switch(response.status) {
					case 'connected':
						deferred.resolve();
						break;
					case 'not authorized':
						deferred.reject();
						break;
					default:
						deferred.reject();
						break;
				}				
			});
		});

		return deferred.promise;
	}

	function initialize() {
		var deferred = $q.defer();

		window.fbAsyncInit = function() {
			deferred.resolve();
		};

		FB.init({
			appId: '770257949654604',
			nativeInterface: CDV.FB,
			useCachedDialogs: false
		});

		if (isFacebookInitialized)
			deferred.resolve();

		return deferred.promise;
	}

	return {
		isUserAuthenticated: isUserAuthenticated,
		initialize: initialize
	};
}]);
