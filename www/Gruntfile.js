module.exports = function(grunt) {

  require('time-grunt')(grunt);

  grunt.initConfig({
    jade: {
      dev: {
        options: {

        },
        files: {
          'index.html': 'index.jade'
        }
      }
    },
    less: {
      dev: {
        options: {
          sourceMap: true
        },
        files: {
          'styles/main.css': 'styles/main.less'
        }
      }
    },
    watch: {
      options: {
        livereload: true
      },
      jade: {
        files: ['*.jade', '**/*.jade'],
        tasks: 'jade:dev'
      },
      less: {
        files: 'styles/**/**/*.less',
        tasks: ['less:dev']
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-jade');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');

  grunt.registerTask('dev', ['less:dev', 'jade:dev', 'watch']);
};
  