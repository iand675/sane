// Load NPM Modules

var express = require('express'),
	jade = require('jade');

// Start an Express application.

var app = express();

// Configure App

app
  .use('view engine', jade)
  .use(express.static(__dirname))
  .use(express.logger());

app.get('/', function(res, res) {
	res.render('index.jade')
});

app.listen(8080, function() {
  console.log("ACTIVE.");
});
