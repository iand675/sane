describe('service.animation', function() {

	var animationService,
		stateServiceMock;

	beforeEach(module('sane'));

	beforeEach(function () {

		stateServiceMock = {
			getCurrentState: function () {},
			getPreviousState: function () {}
		}

		module(function($provide) {
			$provide.value('config', {
				animationOverrides: {
					foo: 'soft'
				}
			});
			$provide.value('stateService', stateServiceMock);
		});

		inject(function($injector) {
			animationService = $injector.get('animationService');
		});
	});

	it('.setBackwardsAnimation should return both backwards entry and departure classes by default.', function () {
		expect(animationService.setBackwardsAnimation()).toEqual('backwards-enter backwards-leave');
	});

	it('.setBackwardsAnimation should return an override class for the previous view if the view is specified in config.', function () {
		spyOn(stateServiceMock, 'getPreviousState').andReturn('foo');
		expect(animationService.setBackwardsAnimation()).toEqual('soft-enter backwards-leave');
	});

	it('.setBackwardsAnimation should return an override class for the current view if the view is specified in config.', function () {
		spyOn(stateServiceMock, 'getCurrentState').andReturn('foo');
		expect(animationService.setBackwardsAnimation()).toEqual('backwards-enter soft-leave');
	});

	it('.setForwardsAnimation should return both forwards entry and departure classes by default.', function () {
		expect(animationService.setForwardsAnimation()).toEqual('forwards-enter forwards-leave');
	});

	it('.setForwardsAnimation should return an override class for the specified view if the view is specified in config.', function () {
		expect(animationService.setForwardsAnimation('foo')).toEqual('soft-enter forwards-leave');
	});

	it('.setForwardsAnimation should return an override class for the current view if the view is specified in config.', function () {
		spyOn(stateServiceMock, 'getCurrentState').andReturn('foo');
		expect(animationService.setForwardsAnimation('bar')).toEqual('forwards-enter soft-leave');
	});

});