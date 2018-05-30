FRAPCON for RAST-K

1. cd build

2. cmake ..

3. Compile   debug version: make
           release version: make -Drelease=true

4. Run test: ./test ../data.inp ../data.out

5. Plot the results:
   cd ../test
   python plot.py