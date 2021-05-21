'use sctrict';

var pg = require('pg');

exports.new_ = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};