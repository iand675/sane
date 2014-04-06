module.exports = function(grunt) {

  require('time-grunt')(grunt);

  grunt.initConfig({
    jade: {
      dev: {
        options: {

        },
        files: {
          'src/index.html': 'src/index.jade'
        }
      }
    },
    less: {
      dev: {
        options: {
          sourceMap: true
        },
        files: {
          'src/styles/main.css': 'src/styles/main.less'
        }
      }
    },
    watch: {
      options: {
        livereload: true
      },
      jade: {
        files: ['src/*.jade', 'src/**/*.jade'],
        tasks: 'jade:dev'
      },
      less: {
        files: 'src/styles/**/**/*.less',
        tasks: ['less:dev']
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-jade');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');

  grunt.registerTask('dev', ['less:dev', 'jade:dev', 'watch']);
};
  
