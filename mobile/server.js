// Load NPM Modules

var express = require('express'),
	jade = require('jade');

// Start an Express application.

var app = express();

// Configure App

app
  .use('view engine', jade)
  .use(express.static(__dirname + '/www'))
  .use(express.logger());

app.get('/', function(res, res) {
	res.render('../index.jade')
});

var port = process.env.PORT || 8080;

app.listen(port, function() {
  console.log("Listening on " + port + ".");
});
