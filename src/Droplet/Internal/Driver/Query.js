'use strict';

var pg = require('pg');

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
    return function (dbHandle) {
        return function (rq) {
            return function (onError, onSuccess) {
                var q = dbHandle.query({
                    name: rq.name,
                    text: rq.text,
                    values: rq.values
                }).then(function (result) {
                    onSuccess(config.right(result.rows));
                }).catch(function (err) {
                    var pgError = config.nullableLeft(err);
                    if (pgError) {
                        onSuccess(pgError);
                    } else {
                        onError(err);
                    }
                });

                return function (_, __, cancelerSuccess) {
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
