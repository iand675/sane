describe('service.phonegap.events', function() {
	var phonegapEventsService,
	rootScope;

	beforeEach(function () {

		module('sane');

		inject(function($injector, _$rootScope_) {
			$rootScope = _$rootScope_;
			phonegapEventsService = $injector.get('phonegapEventsService');
		});
	});

	it('should not be undefined.', function () {
		expect(phonegapEventsService).toBeDefined();
	});

	it('should return a promise when onDeviceReady is called().', function () {
		var returnedPromise = phonegapEventsService.onDeviceReady();

		expect(returnedPromise.then).toBeDefined();
	});

	it('should resolve the promise after the document emits "deviceready".', function () {
		var deviceReady = document.createEvent('CustomEvent'),
			returnedPromise,
			confirmPromiseResolved;

		deviceReady.initCustomEvent('deviceready', true, true);

		returnedPromise = phonegapEventsService.onDeviceReady();

		returnedPromise.then(function () {
			confirmPromiseResolved = true;
		});

		document.dispatchEvent(deviceReady);

		$rootScope.$apply();

		expect(confirmPromiseResolved).toBe(true);
	});
});