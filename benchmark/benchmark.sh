#!/bin/bash

test -d fig2-base || tar zxvf archive.tar.gz

time ../roswell/dirty.ros -y sample.yaml $(find fig2-base -name "*.out")
