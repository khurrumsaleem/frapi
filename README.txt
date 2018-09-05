=========================================================================
==========                                                 ==============
==========   Fuel Rod Analysis Program Interface (FRAPI)   ==============
==========                                                 ==============
=========================================================================

    Dependencies:
        hdf5 (requares in order to run tests)

    1. Make directories 'cd build/debug' or 'cd build/release'

    2. Run 'cmake ../..'

    3. Compile 'make -DCMAKE_BUILD_TYPE=Debug'   for debug version (default)
               'make -DCMAKE_BUILD_TYPE=Release' for release version

    4. Run benchmarks:

        benchmark/IFA-432/test.py
        benchmark/IFA-515/test.py

        The results are the graphics in 'doc/graphics'

    5. Run test for RAST-K:

        5a. Generate input file: 
            cd ../../test
            python data.py

        5b. Run test: ./frapcontest ../../test/data.inp ../../test/data.out

        5c. Plot the results:
            cd ../../test
            python plot.py