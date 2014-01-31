sane.factory('facebookService', ['$q', function ($q) {
	
	function isUserAuthenticated() {
		var deferred = $q.defer();

		initialize().then(function () {
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

	function login() {
		console.log("A");
		initialize().then(function () {
			console.log("B");
			FB.login(function(response) {
			}, {scope: 'email'});
		});
	}

	function initialize() {
		var deferred = $q.defer(),
			checkFb;

		FB.init({
			appId: '770257949654604',
			nativeInterface: CDV.FB,
			useCachedDialogs: false
		});

		checkFb = setTimeout(function () {
			console.log("DSFDSFDS")
			if(window.FB) {
				console.log("HERE")
				deferred.resolve();
				window.clearTimeout(checkFb);
			}
		}, 250);

		return deferred.promise;
	}

	return {
		login: login,
		isUserAuthenticated: isUserAuthenticated,
		initialize: initialize
	};
}]);
