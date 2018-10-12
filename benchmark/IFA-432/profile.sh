#!/bin/bash

frapi='../../build/debug/test_frapi'

#valgrind -v --leak-check=full --show-leak-kinds=all ${frapi} ./IFA-432-r3.inp 1>profile_output.txt 2>profile_output.txt
valgrind -v --leak-check=full --track-origins=yes --show-leak-kinds=all ${frapi} ./IFA-432-r3.inp 1>profile_output.txt 2>profile_output.txt