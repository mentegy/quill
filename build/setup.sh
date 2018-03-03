#!/usr/bin/env bash

echo "Waiting for Postgres"
until psql -h postgres -U postgres -c "SELECT 1" &> /dev/null
do
  printf "."
  sleep 1
done
echo -e "\nPostgres ready"

psql -h postgres -U postgres -c "CREATE DATABASE quill_test"
psql -h postgres -U postgres -d quill_test -a -f quill-sql/src/test/sql/postgres-schema.sql