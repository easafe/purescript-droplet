'use strict';

import pg from 'pg';

export function newPool_(config) {
    return function() {
        return new pg.Pool(config);
    };
}