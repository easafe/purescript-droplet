#!/usr/bin/env bash
# Runs the test suite against a throwaway postgres container, so runs are isolated from local databases and from each other
#
# Extra arguments are passed to spago test, e.g. ./test.sh --offline
set -euo pipefail
cd "$(dirname "$0")"

container="droplet-test-$$-$RANDOM"
image="${DROPLET_TEST_IMAGE:-postgres}"

docker run \
      --detach \
      --rm \
      --name "$container" \
      --env POSTGRES_USER=droplet \
      --env POSTGRES_PASSWORD=droplet \
      --publish 127.0.0.1::5432 \
      "$image" > /dev/null
trap 'docker stop --time 1 "$container" > /dev/null' EXIT

port="$(docker port "$container" 5432/tcp | head -1 | cut -d : -f 2)"

# the postgres image starts a temporary server during initialization, so retry the schema load itself until it succeeds
echo "Waiting for postgres container $container on port $port..."
for _ in $(seq 1 60); do
      if docker exec --interactive "$container" psql --username droplet --dbname droplet --quiet --single-transaction --set ON_ERROR_STOP=1 < test/sql/index.sql 2> /dev/null; then
            ready=1
            break
      fi
      sleep 1
done
if [ -z "${ready:-}" ]; then
      echo "postgres did not become ready" >&2
      exit 1
fi

PGHOST=127.0.0.1 PGPORT="$port" spago test -p droplet "$@"
