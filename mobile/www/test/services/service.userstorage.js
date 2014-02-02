describe('service.userstorage', function() {
	var userStorageService,
		localStorageService,
		$rootScope;

	beforeEach(function () {

		module('sane');

		inject(function($injector, _$rootScope_) {
			$rootScope = _$rootScope_;
			userStorageService = $injector.get('userStorageService');
			localStorageService = $injector.get('localStorageService');
		});

		localStorageService.remove('saneUser');
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
		});

		$rootScope.$apply();

		expect(confirmUserObjectFound).toBe(true);
		expect(saneUser).toEqual({name: 'Test Sane User'});
	});

	it('.checkUserObject() should return a user object if the key "saneUser" is found in localStorage.', function () {
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
		var facebookResponse = {
				accessToken: "foo",
				expiration: "2014-01-27T00:56:08.928Z",
				userId: 10,
				type: "facebook"
			},
			expectedFacebookUserObject = {
				authType: 'facebook',
				facebook: facebookResponse
			},
			saneUser;

		userStorageService.createFacebookUserObject(facebookResponse);

		userStorageService.checkUserObject().then(function (userObject) {
			saneUser = userObject;
		});

		$rootScope.$apply();

		expect(saneUser).toEqual(expectedFacebookUserObject);
	});

	it('.createStandardUserObject() should save a standard user object to localStorage.', function () {
		var standardResponse = {
				email:    "Ian Duncan",
				username: "ian",
				name:     "ian@iankduncan.com"
			},
			expectedStandardUserObject = {
				authType: 'email',
				standard: standardResponse
			},
			saneUser;

		userStorageService.createStandardUserObject(standardResponse);

		userStorageService.checkUserObject().then(function (userObject) {
			saneUser = userObject;
		});

		$rootScope.$apply();

		expect(saneUser).toEqual(expectedStandardUserObject);
	});
});
