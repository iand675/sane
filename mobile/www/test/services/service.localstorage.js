describe('service.localstorage', function() {
	var localStorageService;

	beforeEach(function () {

		module('sane');

		inject(function($injector) {
			localStorageService = $injector.get('localStorageService');
		});
	});

	it('should not be undefined.', function () {
		expect(localStorageService).toBeDefined();
	});

	it('should save and retrieve a null value.', function () {
		var test;

		localStorageService.set('test', null);
		test = localStorageService.get('test');

		expect(test).toEqual(null);
	});

	it('should save and retrieve an integer.', function () {
		var test;

		localStorageService.set('test', 1);
		test = localStorageService.get('test');

		expect(test).toEqual(1);
	});

	it('should save and retrieve a string.', function () {
		var test;

		localStorageService.set('test', 'foo');
		test = localStorageService.get('test');

		expect(test).toEqual('foo');
	});

	it('should save and retrieve an array.', function () {
		var test;

		localStorageService.set('test', ['foo', 'bar']);
		test = localStorageService.get('test');

		expect(test).toEqual(['foo', 'bar']);
	});

	it('should save and retrieve an object.', function () {
		var test;

		localStorageService.set('test', {'foo': 'bar'});
		test = localStorageService.get('test');

		expect(test).toEqual({'foo': 'bar'});
	});

	it('should overwrite objects previously saved to a key.', function () {
		var test;

		localStorageService.set('test', {'foo': 'bar'});
		test = localStorageService.get('test');

		expect(test).toEqual({'foo': 'bar'});

		localStorageService.set('test', {'bar': 'lambda'});
		test = localStorageService.get('test');

		expect(test).toEqual({'bar': 'lambda'});
	});

	it('should remove items saved to particular key.', function () {
		var test;

		localStorageService.set('test', {'foo': 'bar'});
		test = localStorageService.get('test');

		expect(test).toEqual({'foo': 'bar'});

		localStorageService.remove('test');
		test = localStorageService.get('test');

		expect(test).toEqual(undefined);
	});
});