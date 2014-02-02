describe('service.authentication', function() {
	var $httpBackend,
		$q,
		$rootScope,
		configService,
		facebookServiceMock,
		facebookServiceMockPromise,
		userStorageServiceMock,
		userStorageServiceMockPromise,
		authenticationService;

	beforeEach(module('sane'));

	beforeEach(function () {

		facebookServiceMockPromise = {};
		userStorageServiceMockPromise = {};

		facebookServiceMock = {
			isUserAuthenticated: function () {
				facebookServiceMockPromise.isUserAuthenticated = $q.defer();
				return facebookServiceMockPromise.isUserAuthenticated.promise;
			},
			login: function () {
				facebookServiceMockPromise.login = $q.defer();
				return facebookServiceMockPromise.login.promise;
			}
		}

		userStorageServiceMock = {
			checkUserObject: function () {
				userStorageServiceMockPromise.checkUserObject = $q.defer();
				return userStorageServiceMockPromise.checkUserObject.promise;
			}
		}

		module(function ($provide) {
			$provide.value('userStorageService', userStorageServiceMock);
			$provide.value('facebookService', facebookServiceMock);
		});

		inject(function($injector) {
			$rootScope = $injector.get('$rootScope');
			$httpBackend = $injector.get('$httpBackend');
			$q = $injector.get('$q');

			authenticationService = $injector.get('authenticationService');
			configService = $injector.get('configService');
		});
	});

	it('.authenticateNoStrategy should reject if no user object is found and the user is not authenticated via Facebook.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved;

		authenticationServicePromise = authenticationService.authenticateNoStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});
		spyOn(facebookServiceMock, 'isUserAuthenticated').andCallThrough();

		userStorageServiceMockPromise.checkUserObject.reject();
		$rootScope.$apply();
		facebookServiceMockPromise.isUserAuthenticated.reject();
		$rootScope.$apply();

		expect(facebookServiceMock.isUserAuthenticated).toHaveBeenCalled();
		expect(authenticationServicePromiseResolved).toBe(false);
	});

	it('.authenticateNoStrategy should resolve if no user object is found and the user is authenticated via Facebook.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved;

		authenticationServicePromise = authenticationService.authenticateNoStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});
		spyOn(facebookServiceMock, 'isUserAuthenticated').andCallThrough();

		userStorageServiceMockPromise.checkUserObject.reject();
		$rootScope.$apply();
		facebookServiceMockPromise.isUserAuthenticated.resolve();
		$rootScope.$apply();

		expect(facebookServiceMock.isUserAuthenticated).toHaveBeenCalled();
		expect(authenticationServicePromiseResolved).toBe(true);
	});

	it('.authenticateNoStrategy should reject if a user object is found and lacks an authType.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved,
			userObjectMock;

		authenticationServicePromise = authenticationService.authenticateNoStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});

		userStorageServiceMockPromise.checkUserObject.resolve({});
		$rootScope.$apply();

		expect(authenticationServicePromiseResolved).toBe(false);
	});

	it('.authenticateNoStrategy should resolve if a Facebook user object is found and the user is authenticated via Facebook.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved,
			userObjectMock;

		authenticationServicePromise = authenticationService.authenticateNoStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});
		userObjectMock = {
			authType: 'facebook',
			accessToken: "foo",
			expiration: "2014-01-27T00:56:08.928Z",
			userId: 10,
			type: "facebook"
		}
		spyOn(facebookServiceMock, 'isUserAuthenticated').andCallThrough();

		userStorageServiceMockPromise.checkUserObject.resolve(userObjectMock);
		$rootScope.$apply();
		facebookServiceMockPromise.isUserAuthenticated.resolve();
		$rootScope.$apply();

		expect(facebookServiceMock.isUserAuthenticated).toHaveBeenCalled();
		expect(authenticationServicePromiseResolved).toBe(true);
	});

	it('.authenticateNoStrategy should reject if a Facebook user object is found and the user is not authenticated via Facebook.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved,
			userObjectMock;

		authenticationServicePromise = authenticationService.authenticateNoStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});
		userObjectMock = {
			authType: 'facebook',
			accessToken: "foo",
			expiration: "2014-01-27T00:56:08.928Z",
			userId: 10,
			type: "facebook"
		}
		spyOn(facebookServiceMock, 'isUserAuthenticated').andCallThrough();

		userStorageServiceMockPromise.checkUserObject.resolve(userObjectMock);
		$rootScope.$apply();
		facebookServiceMockPromise.isUserAuthenticated.reject();
		$rootScope.$apply();

		expect(facebookServiceMock.isUserAuthenticated).toHaveBeenCalled();
		expect(authenticationServicePromiseResolved).toBe(false);
	});

	it('.authenticateFacebookStrategy should reject if a user is not authenticated via Facebook.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved;

		authenticationServicePromise = authenticationService.authenticateFacebookStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});

		facebookServiceMockPromise.login.reject();
		$rootScope.$apply();

		expect(authenticationServicePromiseResolved).toBe(false);
	});

	it('.authenticateFacebookStrategy should resolve if a user is authenticated via Facebook.', function () {
		var authenticationServicePromise,
			authenticationServicePromiseResolved;

		authenticationServicePromise = authenticationService.authenticateFacebookStrategy().then(function () {
			authenticationServicePromiseResolved = true;
		}, function () {
			authenticationServicePromiseResolved = false;
		});

		$httpBackend.expectPOST(configService.server.signinUri).respond(204);

		facebookServiceMockPromise.login.resolve({});
		$httpBackend.flush();
		$rootScope.$apply();

		expect(authenticationServicePromiseResolved).toBe(true);
	});
});