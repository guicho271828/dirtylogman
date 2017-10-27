#!/bin/bash

test -d fig2-base || tar zxvf archive.tar.gz


find fig2-base/ipc2008-opt-master-ad1e-a333af-2016-05-29-14-55 -name "*.out" | \
    time -v ../roswell/dirty.ros -p -y sample.yaml

# larger db
# find fig2-base -name "*.out" | time -v ../roswell/dirty.ros -p -y sample.yaml
