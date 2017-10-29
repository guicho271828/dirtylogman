#!/bin/bash

test -d fig2-base || tar zxvf archive.tar.gz

touch 0.log
trap "rm 0.log" EXIT

outfile=$((1 + $(basename $(ls *.log | sort -rh | head -n 1) .log)))

# ( echo 1 ; echo 2 >&2 ) >/dev/null 2>&1
# ( echo 1 ; echo 2 >&2 ) 2>&1 >/dev/null
# (( echo 1 ; echo 2 >&2 ) 2>&1 >/dev/null ) >/dev/null

# ros dump executable ../roswell/dirty.ros
# 
# ( find fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55 -name "*.out" | \
#       time -v -o /dev/fd/2 ../roswell/dirty -y sample.yaml 2>&1 >$outfile.res ) |
#     tee $outfile.log


( find fig2-base -name "*.out" | \
      time -v -o /dev/fd/2 ../roswell/dirty.ros -j 12 -y sample.yaml 2>&1 >$outfile.res ) |
    tee $outfile.log

# larger db
# find fig2-base -name "*.out" | time -v ../roswell/dirty.ros -p -y sample.yaml
