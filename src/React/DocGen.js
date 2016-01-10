/* global exports, console */
"use strict";

// module React.DocGen

exports.parse = function(input) {
  var docgen = require('react-docgen');
  return function() {
    return docgen.parse(input);
  };
};
