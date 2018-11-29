#!/bin/bash
cd ./build
make
cd ..
test1=./build/test1
$test1 ./data.inp ./data.out