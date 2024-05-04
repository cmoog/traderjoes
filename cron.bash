#!/usr/bin/env bash

set -exo pipefail

traderjoes fetch
traderjoes gen

sqlite3 traderjoes.db 'select * from items order by inserted_at desc' -csv -header > dump.csv
wrangler pages deploy --project-name traderjoesprices ./site
wrangler r2 object put traderjoesprices-data/dump.csv \
  --file ./dump.csv \
  --cd 'attachment; filename="traderjoes-dump.csv"'
