// This test is, admittedly, brittle as ice. Lesson learned: don't test the implementation, test the output. Refactor pending.

describe('controller.preflight', function () {
	var phonegapEventsServiceMock = {},
		userStorageServiceMock = {},
		preflightControllerServiceMock = {},
		createPreflightController,
		$rootScope, $scope, $q;

	beforeEach(function () {
		module('sane');
	});

	beforeEach(inject(function (_$rootScope_, _$controller_, _$q_) {
		$rootScope = _$rootScope_;
		$scope = _$rootScope_.$new();
		$controller = _$controller_;
		$q = _$q_;

		phonegapEventsServiceMock.onDeviceReady = jasmine.createSpy().andCallFake(function () {
			phonegapEventsServiceMock.deferred = $q.defer();
			return phonegapEventsServiceMock.deferred.promise;
		});

		userStorageServiceMock.checkUserObject = jasmine.createSpy().andCallFake(function () {
			userStorageServiceMock.deferred = $q.defer();
			return userStorageServiceMock.deferred.promise;
		});

		preflightControllerServiceMock = jasmine.createSpyObj('preflightControllerServiceMock', ['checkAuthenticationForLogin', 'checkConnectionForLogin']);

        createPreflightController = function () {
        	return $controller('preflightController', {
	        	$scope: $scope,
	            phonegapEventsService: phonegapEventsServiceMock,
	            userStorageService: userStorageServiceMock,
	            preflightControllerService: preflightControllerServiceMock
        	});
        }
	}));

	it('should call onDeviceReady upon resolution of deviceReady event from phonegap.events service.', function () {
		var preflightController = createPreflightController();

		spyOn($scope, 'onDeviceReady').andCallThrough();

		phonegapEventsServiceMock.deferred.resolve();

		$rootScope.$apply();

		expect(phonegapEventsServiceMock.onDeviceReady).toHaveBeenCalled();
		expect($scope.onDeviceReady).toHaveBeenCalled();
	});

	it('should call onUserObjectFound if userStorageService discovers a user object.', function () {
		var preflightController = createPreflightController();

		spyOn($scope, 'onUserObjectFound').andCallThrough();

		phonegapEventsServiceMock.deferred.resolve();

		$rootScope.$apply();

		userStorageServiceMock.deferred.resolve();

		$rootScope.$apply();

		expect($scope.onUserObjectFound).toHaveBeenCalled();
	});

	it('should not call onUserObjectNotFound if userStorageService discovers a user object.', function () {
		var preflightController = createPreflightController();

		spyOn($scope, 'onUserObjectNotFound').andCallThrough();

		phonegapEventsServiceMock.deferred.resolve();

		$rootScope.$apply();

		userStorageServiceMock.deferred.resolve();

		$rootScope.$apply();

		expect($scope.onUserObjectNotFound).not.toHaveBeenCalled();
	});

	it('should call onUserObjectNotFound if userStorageService does not discover a user object.', function () {
		var preflightController = createPreflightController();

		spyOn($scope, 'onUserObjectNotFound').andCallThrough();

		phonegapEventsServiceMock.deferred.resolve();

		$rootScope.$apply();

		userStorageServiceMock.deferred.reject();

		$rootScope.$apply();

		expect($scope.onUserObjectNotFound).toHaveBeenCalled();
	});

	it('should not call onUserObjectFound if userStorageService does not discover a user object.', function () {
		var preflightController = createPreflightController();

		spyOn($scope, 'onUserObjectFound').andCallThrough();

		phonegapEventsServiceMock.deferred.resolve();

		$rootScope.$apply();

		userStorageServiceMock.deferred.reject();

		$rootScope.$apply();

		expect($scope.onUserObjectFound).not.toHaveBeenCalled();
	});

	it('should call checkAuthenticationForLogin after onUserObjectFound is called.', function () {
		var preflightController = createPreflightController();

		$scope.onUserObjectFound();

		expect(preflightControllerServiceMock.checkAuthenticationForLogin).toHaveBeenCalled();
	});

	it('should call checkConnectionForLogin after onUserObjectNotFound is called.', function () {
		var preflightController = createPreflightController();

		$scope.onUserObjectNotFound();

		expect(preflightControllerServiceMock.checkConnectionForLogin).toHaveBeenCalled();
	});
})