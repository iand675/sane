// Karma configuration
// Generated on Mon Jan 27 2014 12:04:40 GMT-0800 (PST)

module.exports = function(config) {
  config.set({

    // base path, that will be used to resolve files and exclude
    basePath: '',


    // frameworks to use
    frameworks: ['jasmine'],


    // list of files / patterns to load in the browser
    files: [
        'www/js/lib/lodash/dist/lodash.min.js',
        'www/js/lib/fastclick/lib/fastclick.js',
        'www/js/lib/lawnchair/index.js',
        'www/js/lib/angular/angular.js',
        'www/js/lib/angular-mocks/angular-mocks.js',
        'www/js/lib/angular-ui-router/release/angular-ui-router.min.js',
        'www/js/lib/angular-animate/angular-animate.min.js',
        'www/js/sane.js',
        'www/js/modules/sane.services.js',
        'www/js/modules/sane.controllers.js',
        'www/js/modules/sane.directives.js',
        'www/js/modules/sane.routing.js',
        'www/js/services/**/*.js',
        'www/js/controllers/**/*.js',
        'www/test/**/*.js'
    ],


    // list of files to exclude
    exclude: [
        'www/js/lib/angular/angular-animate/**/*.js',
        'www/js/lib/angular/angular-ui-router/**/*.js'
    ],


    // test results reporter to use
    // possible values: 'dots', 'progress', 'junit', 'growl', 'coverage'
    reporters: ['progress'],


    // web server port
    port: 9999,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,


    // Start these browsers, currently available:
    // - Chrome
    // - ChromeCanary
    // - Firefox
    // - Opera (has to be installed with `npm install karma-opera-launcher`)
    // - Safari (only Mac; has to be installed with `npm install karma-safari-launcher`)
    // - PhantomJS
    // - IE (only Windows; has to be installed with `npm install karma-ie-launcher`)
    browsers: ['PhantomJS'],


    // If browser does not capture in given timeout [ms], kill it
    captureTimeout: 60000,


    // Continuous Integration mode
    // if true, it capture browsers, run tests and exit
    singleRun: false
  });
};
