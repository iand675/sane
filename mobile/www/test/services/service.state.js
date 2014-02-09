describe('service.state', function () {
	var stateService,
		stateMock;

	beforeEach(module('sane'));

	beforeEach(function () {
		stateMock = {
			current: {
				name: 'foo'
			},
			go: function (state) {
				return;
			}
		}

		module(function ($provide) {
			$provide.value('$state', stateMock);
		});

		inject(function ($injector) {
			stateService = $injector.get('stateService');
		})
	});

	it('.getCurrentState should return the current state.', function () {
		var currentState = stateService.getCurrentState();

		expect(currentState).toEqual('foo');
	});

	it('.getPreviousState should return the first item in the stateHistory array.', function () {
		var previousState;

		stateService.setStateHistory(['bar', 'foo', 'baz']);

		previousState = stateService.getPreviousState();

		expect(previousState).toEqual('bar');
	});

	it('.goBack should initiate a transition to a previous state if it exists.', function () {
		stateService.setStateHistory(['bar', 'foo', 'baz']);

		spyOn(stateMock, 'go');

		stateService.goBack();

		expect(stateMock.go).toHaveBeenCalledWith('bar');
		expect(stateService.getStateHistory()).toEqual(['foo', 'baz']);
	});

	it('.goBack should not initiate a transition to a previous state if one does not exist.', function () {
		spyOn(stateMock, 'go');

		stateService.goBack();

		expect(stateMock.go).not.toHaveBeenCalled();
	});

	it('.from should add a state to the beginning of stateHistory if it has not already been added.', function () {
		stateService.from('bar');

		expect(stateService.getStateHistory()).toEqual(['bar']);
	});

	it('.from should not add a state to the beginning of stateHistory if it has already been added.', function () {
		stateService.setStateHistory(['bar'])

		stateService.from('bar');

		expect(stateService.getStateHistory()).toEqual(['bar']);
	});

	it('.goTo should initiate a transition to another state.', function () {
		spyOn(stateMock, 'go');

		stateService.goTo('foo');

		expect(stateMock.go).toHaveBeenCalledWith('foo');
	});
});