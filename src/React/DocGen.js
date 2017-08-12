/* global exports, console */
"use strict";

exports.parseImpl = function(input) {
  var docgen = require('react-docgen');
  return function() {
    return docgen.parse(input);
  };
};
