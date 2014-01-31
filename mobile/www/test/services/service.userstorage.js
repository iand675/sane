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
});