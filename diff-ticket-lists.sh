#!/usr/bin/env bash

set -e

psql trac_ghc -c "\\copy (select id from ticket order by id) to '/tmp/trac_ids.csv';"
psql gitlab   -c "\\copy (select iid from issues where project_id = 1 order by iid) to '/tmp/gitlab_ids.csv';"
diff -u /tmp/trac_ids.csv /tmp/gitlab_ids.csv
