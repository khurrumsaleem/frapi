Module fuelrod

Dependencies:
   frapcon

1. cd build/debug 
   or
   cd build/release

2. cmake ../..

3. Compile   debug version: make
           release version: make -Drelease=true

4. Generate input file: 
   cd ../../test
   python data.py

5. Run test: ./frapcontest ../../test/data.inp ../../test/data.out

6. Plot the results:
   cd ../../test
   python plot.py

TODO:
1. Extend the list of output variables
2. Inlet temperature
3. Supress output from frapcon
4. Check failure of the fuel rod
5. HDF5 for restart
6. Parallelization with MPI or ZMQ