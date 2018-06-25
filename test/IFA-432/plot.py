from h5py import *
from numpy import *
import matplotlib.pyplot as plt

f0 = File('432r1.h5')
f1 = File('plot432r1.h5')

names = [
#'centerline temperature, C',
#'fission gas release, %',
#'average linear power, W|cm',
#'bulk coolant temperature, C',
#'total gap conductance, W|(m^2*K)',
#'oxide thickness, um',
#'mechanical gap thickness, um',
#'gap pressure, MPa',
#'cladding hoop strain, %',
#'cladding hoop stress, MPa',
'fuel burnup, MW*s|kg',
]   

for i, name in enumerate(names):
    fig, ax = plt.subplots()

    time0 , data0 = [], []
    for group in f0.keys():
        time0.append(f0[group]['time, day'][0])
        data0.append(f0[group][name][0])

    time1 , data1 = [], []    
    for group in f1.keys():        
        time1.append(f1[group]['time, day'])
        data1.append(f1[group][name][0]) 

    ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
    ax.plot(time1, data1, '-o', ms=1, lw=2, alpha=0.7, mfc='blue')
    ax.set_xlabel('Time, day')
    ax.set_ylabel(name)
    ax.legend(['NEW FRAPCON', 'OLD FRAPCON'])
    ax.grid()
#plt.savefig('../doc/figs/%s.png'%name.split(',')[0].replace(' ','_'))

plt.show()

f0.close()
f1.close()