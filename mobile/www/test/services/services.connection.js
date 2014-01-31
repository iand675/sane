describe('service.connection', function() {
	var $httpBackend,
		$timeout,
		$q,
		$rootScope,
		connectionService;

	beforeEach(function () {

		module('sane');

		inject(function($injector, _$rootScope_, _$httpBackend_, _$timeout_, _$q_) {
			$rootScope = _$rootScope_;
			$httpBackend = _$httpBackend_;
			$timeout = _$timeout_;
			$q = _$q_;

			connectionService = $injector.get('connectionService');
		});
	});

	it('should be defined.', function () {
		expect(connectionService).toBeDefined();
	});

	it('.checkConnectionToServer() should ping the Sane server.', function () {
		$httpBackend.expectGET('https://saneapp.com/ping');

		connectionService.checkConnectionToServer();

	});

	it('.checkConnectionToServer() should call success if response is received.', function () {
		var confirmPromiseResolved;

		$httpBackend.expectGET('https://saneapp.com/ping').respond(204, 'OK');

		connectionService.checkConnectionToServer().then(function () {
			confirmPromiseResolved = true;
		}, function () {
			confirmPromiseResolved = false;
		});

		$httpBackend.flush();
		$rootScope.$apply();

		expect(confirmPromiseResolved).toBe(true);
	});

	it('.checkConnectionToServer() should call error if the server returns a 503.', function () {
		var confirmPromiseResolved;

		$httpBackend.expectGET('https://saneapp.com/ping').respond(503, 'OK');

		connectionService.checkConnectionToServer().then(function () {
			confirmPromiseResolved = true;
		}, function () {
			confirmPromiseResolved = false;
		});

		$httpBackend.flush();
		$rootScope.$apply();

		expect(confirmPromiseResolved).toBe(false);
	});
});