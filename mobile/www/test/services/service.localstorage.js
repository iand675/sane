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

		localStorageService.setItem('test', null);
		test = localStorageService.getItem('test');

		expect(test).toEqual(null);
	});

	it('should save and retrieve an integer.', function () {
		var test;

		localStorageService.setItem('test', 1);
		test = localStorageService.getItem('test');

		expect(test).toEqual(1);
	});

	it('should save and retrieve a string.', function () {
		var test;

		localStorageService.setItem('test', 'foo');
		test = localStorageService.getItem('test');

		expect(test).toEqual('foo');
	});

	it('should save and retrieve an array.', function () {
		var test;

		localStorageService.setItem('test', ['foo', 'bar']);
		test = localStorageService.getItem('test');

		expect(test).toEqual(['foo', 'bar']);
	});

	it('should save and retrieve an object.', function () {
		var test;

		localStorageService.setItem('test', {'foo': 'bar'});
		test = localStorageService.getItem('test');

		expect(test).toEqual({'foo': 'bar'});
	});

	it('should overwrite objects previously saved to a key.', function () {
		var test;

		localStorageService.setItem('test', {'foo': 'bar'});
		test = localStorageService.getItem('test');

		expect(test).toEqual({'foo': 'bar'});

		localStorageService.setItem('test', {'bar': 'lambda'});
		test = localStorageService.getItem('test');

		expect(test).toEqual({'bar': 'lambda'});
	});

	it('should remove items saved to particular key.', function () {
		var test;

		localStorageService.setItem('test', {'foo': 'bar'});
		test = localStorageService.getItem('test');

		expect(test).toEqual({'foo': 'bar'});

		localStorageService.remove('test');
		test = localStorageService.getItem('test');

		expect(test).toEqual(undefined);
	});
});