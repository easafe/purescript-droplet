'use sctrict';

var pg = require('pg');

exports.newPool_ = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};