module.exports = function(grunt) {

	require('time-grunt')(grunt);

	grunt.initConfig({
		clean: {
			splashscreen: ['platforms/ios/Sane/Resources/splash/**/*']
		},
		copy: {
			splashscreen: {
				files: [
					{
						expand: true,
						cwd: 'www/res/screen/ios/',
						src: ['*'],
						dest: 'platforms/ios/Sane/Resources/splash'
					}
				]
			}
		},
		shell: {
			'build-ios': {
				options: {
					stdout: true
				},
				command: 'phonegap build ios'
			},
			'install-ios': {
				options: {
					stdout: true
				},
				command: 'phonegap install ios'	
			},
			'init': {
				options: {
					stdout: true
				},
				command: 'bower install'
			}
		}
	});

	grunt.loadNpmTasks('grunt-shell');
	grunt.loadNpmTasks('grunt-contrib-copy');
	grunt.loadNpmTasks('grunt-contrib-clean');

	grunt.registerTask('init', ['shell:init'])
	grunt.registerTask('transfer-spashscreens', ['clean:splashscreen', 'copy:splashscreen']);
	grunt.registerTask('emulate', ['shell:build-ios', 'transfer-spashscreens', 'shell:install-ios']);
}