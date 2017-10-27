#!/bin/bash

test -d fig2-base || tar zxvf archive.tar.gz

touch 0.log
trap "rm 0.log" EXIT

outfile=$((1 + $(basename $(ls *.log | sort -rh | head -n 1) .log))).log

# ( echo 1 ; echo 2 >&2 ) >/dev/null 2>&1
# ( echo 1 ; echo 2 >&2 ) 2>&1 >/dev/null
# (( echo 1 ; echo 2 >&2 ) 2>&1 >/dev/null ) >/dev/null


( find fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55 -name "*.out" | \
      time -v -o /dev/fd/2 ../roswell/dirty.ros -p -y sample.yaml 2>&1 >/dev/null ) |
    tee $outfile

# larger db
# find fig2-base -name "*.out" | time -v ../roswell/dirty.ros -p -y sample.yaml
