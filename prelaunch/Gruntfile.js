module.exports = function(grunt) {

	require('time-grunt')(grunt);

	grunt.initConfig({
		stylus: {
			dev: {
				files: {
					'index.css': ['stylus/*.styl']
				}
			}
		},
		jade: {
			dev: {
				files: {
					'index.html': ['jade/*.jade']
				}
			}
		},
		watch: {
			stylus: {
				files: ['stylus/*.styl'],
				tasks: ['stylus']
			},
			jade: {
				files: ['jade/*.jade'],
				tasks: ['jade']
			},
			options: {
				livereload: true
			}
		}
	});

	grunt.loadNpmTasks('grunt-contrib-stylus');
	grunt.loadNpmTasks('grunt-contrib-jade');
	grunt.loadNpmTasks('grunt-contrib-watch');

	grunt.registerTask('dev', ['stylus:dev', 'jade:dev', 'watch'])
}