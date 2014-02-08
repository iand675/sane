describe('service.userstorage', function() {
	var userStorageService,
		localStorageService,
		localStorageMock,
		localStorageServiceMock,
		$rootScope;

	beforeEach(module('sane'));

	beforeEach(function () {

		localStorageMock = {};

		localStorageServiceMock = {
			getItem: function (key) {
				return localStorageMock[key];
			},
			setItem: function (key, object) {
				localStorageMock[key] = object;
			},
			remove: function (key) {
				delete localStorageMock[key];
			}
		}

		module(function ($provide) {
			$provide.value('localStorageService', localStorageServiceMock);
		});

		inject(function($injector, _$rootScope_) {
			$rootScope = _$rootScope_;
			userStorageService = $injector.get('userStorageService');
			localStorageService = $injector.get('localStorageService');
		});
	});

	it('should be defined.', function () {
		expect(userStorageService).toBeDefined();
	});

	it('.checkUserObject() should return a user object if the key "saneUser" is found in localStorage.', function () {
		var confirmUserObjectFound,
			saneUser;

		localStorageService.setItem('saneUser', {name: 'Test Sane User'});

		userStorageService.checkUserObject().then(function (userObject) {
			saneUser = userObject;
			confirmUserObjectFound = true;
		}, function () {
			confirmUserObjectFound = false;
		});

		$rootScope.$apply();

		expect(confirmUserObjectFound).toBe(true);
		expect(saneUser).toEqual({name: 'Test Sane User'});
	});

	it('.checkUserObject() should reject if an object with the key "saneUser" is not found in localStorage.', function () {
		var confirmUserObjectFound;

		userStorageService.checkUserObject().then(function () {
			confirmUserObjectFound = true;
		}, function () {
			confirmUserObjectFound = false;
		});

		$rootScope.$apply();

		expect(confirmUserObjectFound).toBe(false);
	});

	it('.createFacebookUserObject() should save a Facebook user object to localStorage.', function () {
		var authResponse = {
				accessToken: "foo",
				expiration: "2014-01-27T00:56:08.928Z",
				userId: 10,
				type: "facebook"
			},
			data = {
				name: "foo"
			},
			expectedFacebookUserObject = {
				authType: 'facebook',
				credentials: authResponse,
				data: data
			},
			saneUser;

		userStorageService.createFacebookUserObject(authResponse, data);

		userStorageService.checkUserObject().then(function (userObject) {
			saneUser = userObject;
		});

		$rootScope.$apply();

		expect(saneUser).toEqual(expectedFacebookUserObject);
	});

	it('.createStandardUserObject() should save a standard user object to localStorage.', function () {
		var standardResponse = {
				email:    "Ian Duncan",
				username: "iandemo",
				name:     "ian@iankduncan.com"
			},
			credentials = {
				email: 'ian@iankduncan.com',
				password: 'blah'
			},
			expectedStandardUserObject = {
				authType: 'email',
				credentials: credentials,
				data: standardResponse
			},
			saneUser;

		userStorageService.createStandardUserObject(credentials, standardResponse);

		userStorageService.checkUserObject().then(function (userObject) {
			saneUser = userObject;
		});

		$rootScope.$apply();

		expect(saneUser).toEqual(expectedStandardUserObject);
	});
});
