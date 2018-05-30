from numpy import *
import matplotlib.pyplot as plt

table = loadtxt('1.inp', skiprows=14)

distortion = lambda t: 1. + 0.1 * sin(pi*t/50.)

time = linspace(0,1000.,51.)
data = array([table[1:]]).T[0] * distortion(time)
data = vstack([time, data])

#f = open('data2.inp', 'w')

savetxt('2_.inp',data,fmt='%11.7f')

#for d in data:
#	f.write('{:.6}'.format(d))

