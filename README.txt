=========================================================================
==========                                                 ==============
==========   Fuel Rod Analysis Program Interface (FRAPI)   ==============
==========                                                 ==============
=========================================================================

    Dependencies:
        numpy
        hdf5py (requares in order to run tests)

    1. Make directories 'cd build/debug' or 'cd build/release'

    2. Compile 'cmake ../.. -DCMAKE_BUILD_TYPE=Debug'   for debug version (default)
               'cmake ../.. -DCMAKE_BUILD_TYPE=Release' for release version

    3. Compile 'make'

    4. Run benchmarks:

        benchmark/IFA-432/test.py
        benchmark/IFA-515/test.py

        The results are the graphics in 'doc/graphics'

    5. Run example:

        benchmark/example/test.py
