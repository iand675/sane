sane.factory('animationService', ['stateService', 'configService', function (stateService, configService) {
	var enterAnimation,
		leaveAnimation;

	function setBackwardsAnimation() {
		enterAnimation = (configService.animationOverrides[stateService.getPreviousState()] || 'backwards') + '-enter';
		leaveAnimation = (configService.animationOverrides[stateService.getCurrentState()] || 'backwards') + '-leave';

		return enterAnimation + ' ' + leaveAnimation;
	}

	function setForwardsAnimation(state) {
		enterAnimation = (configService.animationOverrides[state] || 'forwards') + '-enter';
		leaveAnimation = (configService.animationOverrides[stateService.getCurrentState()] || 'forwards') + '-leave';

		return enterAnimation + ' ' + leaveAnimation;
	}

	return {
		setForwardsAnimation: setForwardsAnimation,
		setBackwardsAnimation: setBackwardsAnimation
	};
}]);