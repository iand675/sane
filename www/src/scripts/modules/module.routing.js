sane.routing = angular.module('sane.routing', [
  'ui.router'
]).config(function ($stateProvider, $urlRouterProvider) {

  function goPikabu() {
    setTimeout(function () {
      var pikabu = new Pikabu({
        viewportSelector: '[data-pikabu-element="viewport"]',
        selectors: {
          element: '[data-pikabu-element="container"]',
          common: '[data-pikabu-element="sidebar"]',
          left: '[data-pikabu-sidebar="left"]',
          right: '[data-pikabu-sidebar="right"]',
          navToggles: '[data-pikabu-element="nav-toggle"]'
        },
        widths: {
          left: '80%',
          right: '80%'
        }
      })
    }, 1000);
  }

  $stateProvider
    .state('main', {
      templateUrl: 'templates/viewport.html'
    })
    .state('main.home', {
      url: '/home',
      views: {
        'main': { templateUrl: 'templates/main/home.html' },
        'sidebar-left': { templateUrl: 'templates/sidebar/lists.html' },
        'sidebar-right': { templateUrl: 'templates/sidebar/talk.html' }
      },
      onEnter: function () {
        goPikabu();
      }
    })
    .state('main.home-pulse', {
      views: {
        'main': { templateUrl: 'templates/main/home.html' },
        'sidebar-left': { templateUrl: 'templates/sidebar/lists.html' },
        'sidebar-right': { templateUrl: 'templates/sidebar/pulse.html' }
      },
      onEnter: function () {
        goPikabu();
      }
    })
    .state('main.home-talk', {
      views: {
        'main': { templateUrl: 'templates/main/home.html' },
        'sidebar-left': { templateUrl: 'templates/sidebar/lists.html' },
        'sidebar-right': { templateUrl: 'templates/sidebar/talk.html' }
      },
      onEnter: function () {
        goPikabu();
      }
    })
});