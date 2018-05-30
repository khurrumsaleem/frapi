Module fpn4rastk

Requirements:
   frapcon

1. cd build

2. cmake ..

3. Compile   debug version: make
           release version: make -Drelease=true

4. Run test: ./frapcontest ../test/data.inp ../test/data.out

5. Plot the results:
   cd ../test
   python plot.py