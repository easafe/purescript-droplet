'use strict';

// `pg related code/bindings are done here as we want to
// allow frontend modules to access `PostgreSQL.*` classes too.
// Putting this import into `PostgreSQL/Value.js` caused is a problem
// for the web bundlers etc.

var pg = require('pg');

// pg does strange thing converting DATE
// value to js Date, so we have
// to prevent this craziness
pg.types.setTypeParser(1082 /* DATE_OID */, function (dateString) { return dateString; });

exports.connect_ = function (config) {
    return function (pool) {
        return function (onError, onSuccess) {
            var p = pool.connect().then(function (client) {
                onSuccess(config.right({
                    client: client,
                    done: function () {
                        return client.release();
                    }
                }));
            }).catch(function (err) {
                var pgError = config.nullableLeft(err);
                if (pgError) {
                    onSuccess(pgError);
                } else {
                    onError(err);
                }
            });

            return function (cancelError, cancelerError, cancelerSuccess) {
                p.cancel();
                cancelerSuccess();
            };
        };
    };
};

exports.rawQuery_ = function (config) {
    // Either `Pool` or `Client` instance
    return function (dbHandle) {
        return function (rq) {
            return function (onError, onSuccess) {
                var q = dbHandle.query({
                    name: rq.name,
                    text: rq.text,
                    values: rq.values
                }).then(function (result) {
                    onSuccess(config.right(result));
                }).catch(function (err) {
                    var pgError = config.nullableLeft(err);
                    if (pgError) {
                        onSuccess(pgError);
                    } else {
                        onError(err);
                    }
                });

                return function (cancelError, cancelerError, cancelerSuccess) {
                    q.cancel();
                    cancelerSuccess();
                };
            };
        };
    };
};

exports.sqlState_ = function (error) {
    return error.code || null;
};

exports.errorDetail_ = function (error) {
    return {
        error: error,
        severity: error.severity || '',
        code: error.code || '',
        message: error.message || '',
        detail: error.detail || '',
        hint: error.hint || '',
        position: error.position || '',
        internalPosition: error.internalPosition || '',
        internalQuery: error.internalQuery || '',
        where_: error.where || '',
        schema: error.schema || '',
        table: error.table || '',
        column: error.column || '',
        dataType: error.dataType || '',
        constraint: error.constraint || '',
        file: error.file || '',
        line: error.line || '',
        routine: error.routine || ''
    };
};
