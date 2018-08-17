#########################################################################
##########                                                 ##############
##########  Fuel Rod Performance Analysis Interface FRAPI  ##############
##########                                                 ##############
#########################################################################

    Dependencies:
        frapcon
        fraptran
        hdf5 (requares in order to run tests)

    1. 'cd build/debug' or 'cd build/release'

    2. 'cmake ../..'

    3. Compile 'make'                (debug)
               'make -Drelease=true' (release)

    4. Run tests:
        test/IFA-432/test.py
        test/IFA-512/test.py
        The results are graphics in doc/manual/figs

    5. Run test for RAST-K:

        5a. Generate input file: 
            cd ../../test
            python data.py

        5b. Run test: ./frapcontest ../../test/data.inp ../../test/data.out

        5c. Plot the results:
            cd ../../test
            python plot.py