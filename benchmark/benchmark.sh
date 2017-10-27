#!/bin/bash

test -d fig2-macro || tar zxvf fig2-macro.tar.gz

time ../roswell/dirty.ros -y sample.yaml $(find fig2-macro -name "*.out")
