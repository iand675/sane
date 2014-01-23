module.exports = function(grunt) {

	require('time-grunt')(grunt);

	grunt.initConfig({
		clean: {
			splashscreen: ['platforms/ios/Sane/Resources/splash/**/*'],
			icon: ['platforms/ios/Sane/Resources/icons/**/*']
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
			},
			icon: {
				files: [
					{
						expand: true,
						cwd: 'www/res/icon/ios/',
						src: ['*'],
						dest: 'platforms/ios/Sane/Resources/icons'
					}
				]
			}
		},
		jade: {
			dev: {
				files: {
					"www/index.html": "www/index.jade",
					"www/templates/welcome.html": "www/templates/welcome.jade",
					"www/templates/.html": "www/templates/login.jade"
				}
			}
		},
		jshint: {
			dev: {
				options: {
					ignores: ['www/js/lib/**/*.js']
				},
				files: {
					src: ['www/js/*.js']
				}
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
		},
		less: {
			dev: {
				options: {
				},
				files: {
					"www/styles/index.css": ["www/styles/less/**/*.less", "www/styles/less/general.less"]
				}
			}	
		},
		watch: {
			jade: {
				files: ['www/index.jade', 'www/templates/*.jade'],
				tasks: ['jade:dev']
			},
			jshint: {
				files: ['www/js/*.js'],
				tasks: ['jshint:dev']
			},
			stylus: {
				files: ['www/styles/less/**/*.less'],
				tasks: ['less:dev']
			}
		}
	});

	grunt.loadNpmTasks('grunt-shell');
	grunt.loadNpmTasks('grunt-contrib-copy');
	grunt.loadNpmTasks('grunt-contrib-clean');
	grunt.loadNpmTasks('grunt-contrib-jade');
	grunt.loadNpmTasks('grunt-contrib-jshint');
	grunt.loadNpmTasks('grunt-contrib-less');
	grunt.loadNpmTasks('grunt-contrib-watch');

	grunt.registerTask('init', ['shell:init']);
	grunt.registerTask('dev', ['jade:dev', 'less:dev', 'watch'])
;	grunt.registerTask('transfer-resources', ['clean:splashscreen', 'copy:splashscreen', 'clean:icon', 'copy:icon']);
	grunt.registerTask('emulate', ['shell:build-ios', 'transfer-resources', 'shell:install-ios']);
}