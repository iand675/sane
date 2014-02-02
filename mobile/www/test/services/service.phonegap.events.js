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

	it('should be defined.', function () {
		expect(phonegapEventsService).toBeDefined();
	});

	it('should resolve the promise after the document emits "deviceready".', function () {
		var deviceReady = document.createEvent('CustomEvent'),
			confirmPromiseResolved;

		deviceReady.initCustomEvent('deviceready', true, true);

		phonegapEventsService.onDeviceReady().then(function () {
			confirmPromiseResolved = true;
		});

		document.dispatchEvent(deviceReady);

		$rootScope.$apply();

		expect(confirmPromiseResolved).toBe(true);
	});
});
