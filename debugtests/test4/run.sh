#!/bin/bash
cd ./build
rm test2
make
cd ..
test2=./build/test2
fraptran=../../build/debug/main_fraptran
plot2h5=./plot2h5.py
#$test2 ./data.inp ./data.out
$fraptran test.inp
$plot2h5 test.plot test.h5