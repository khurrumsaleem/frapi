Module frapi

Dependencies:
   frapcon
   fraptran
   hdf5

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
1. Extend the list of output variables ...done!
2. Inlet temperature and pressure ...done!
3. Supress output from frapcon ...done!
4. Check fuel rod failure conditions
5. HDF5 or binary file for restart ...done!
6. Parallelization with MPI or ZMQ
7. Deallocation of FRAPCON variables ...done!
8. Memory cost reduction ...? (now it is about 7 KB per fuel rod)
9. Double the fuel rod stack ...? (will increase memory costs)
10. Fix error with the initial state fuel temperature ...done!
11. Check isotopes concentrations