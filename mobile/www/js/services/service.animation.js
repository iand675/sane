sane.services.factory('animationService', ['stateService', 'config', function (stateService, config) {
	var enterAnimation,
		leaveAnimation;

	function setBackwardsAnimation() {
		enterAnimation = (config.animationOverrides[stateService.getPreviousState()] || 'backwards') + '-enter';
		leaveAnimation = (config.animationOverrides[stateService.getCurrentState()] || 'backwards') + '-leave';

		return enterAnimation + ' ' + leaveAnimation;
	}

	function setForwardsAnimation(state) {
		enterAnimation = (config.animationOverrides[state] || 'forwards') + '-enter';
		leaveAnimation = (config.animationOverrides[stateService.getCurrentState()] || 'forwards') + '-leave';

		return enterAnimation + ' ' + leaveAnimation;
	}

	return {
		setForwardsAnimation: setForwardsAnimation,
		setBackwardsAnimation: setBackwardsAnimation
	};
}]);