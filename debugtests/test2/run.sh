#!/bin/bash
cd ./build
rm test1
make
cd ..
test1=./build/test1
$test1 ./data.inp ./data.out