#!/bin/bash

test -d fig2-base || tar zxvf archive.tar.gz

find fig2-base -name "*.out" | time -v ../roswell/dirty.ros -p -y sample.yaml
