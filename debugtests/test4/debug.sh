#!/bin/bash
cd ./build
rm test2
make
cd ..

test2=./build/test2

gdb --command script.gdb --args $test2 ./data.inp ./data.out

