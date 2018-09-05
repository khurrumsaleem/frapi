from h5py import *
from numpy import *
import matplotlib.pyplot as plt

f = File('data.h5')

names = [
'axial fuel temperature, C',
'bulk coolant temperature, C',
#'total gap conductance, W/(m^2*K)',
'oxide thickness, um',
'mechanical gap thickness, um',
'gap pressure, MPa',
'cladding hoop strain, %',
'cladding axial stress, MPa',
]   

for i, name in enumerate(names):
    if i in [6]:    
        fig, ax = plt.subplots()
        for group in f.keys()[::5]:
            data = f[group][name] 
            mesh = f[group]['axial mesh, cm']
            ax.plot(mesh, data, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.set_xlabel('Axial elevation, cm')
        ax.set_ylabel(name)
        #ax.legend(['NEW FRAPCON', 'OLD FRAPCON'])
        ax.grid()
    #plt.savefig('../doc/figs/%s.png'%name.split(',')[0].replace(' ','_'))

plt.show()

f.close()