#!/bin/bash

test -d fig2-macro || tar zxvf fig2-macro.tar.gz

dirty -y sample.yaml $(find -name "*.out" fig2-macro)
