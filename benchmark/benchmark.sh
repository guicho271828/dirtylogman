#!/bin/bash

test -d fig2-base || tar zxvf archive.tar.gz

find fig2-base -name "*.out" | time ../roswell/dirty.ros -y sample.yaml
