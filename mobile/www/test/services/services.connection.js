describe('service.connection', function() {
	var $httpBackend,
		$q,
		$rootScope,
		configService,
		connectionService;

	beforeEach(function () {

		module('sane');

		inject(function($injector) {
			$rootScope = $injector.get('$rootScope');
			$httpBackend = $injector.get('$httpBackend');
			$q = $injector.get('$q');

			connectionService = $injector.get('connectionService');
			configService = $injector.get('configService');
		});
	});

	it('.checkConnectionToServer() should ping the Sane server.', function () {
		$httpBackend.expectGET(configService.server.pingUri);

		connectionService.checkConnectionToServer();

	});

	it('.checkConnectionToServer() should call success if response is received.', function () {
		var confirmPromiseResolved;

		$httpBackend.expectGET(configService.server.pingUri).respond(204);

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

		$httpBackend.expectGET(configService.server.pingUri).respond(503);

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
