#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call

call(['../../build/debug/rastk_input_file', 'data.inp', 'data.out'])

import plot