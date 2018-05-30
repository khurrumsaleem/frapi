from numpy import *
import matplotlib.pyplot as plt

tcool = array([296.6948314, 298.8020999, 301.5829215, 304.7806656, 307.9590066, 310.8991638, 313.8582962, 
	316.4751255, 318.5760391, 320.5040240, 321.5913390, 322.4330218])

pcool = array([15.6443709, 15.6382882, 15.6231352, 15.6167805, 15.6014046, 15.5858559, 15.5794307, 15.5636761, 
	15.5477487, 15.5413040, 15.5252308, 15.5189934])

ql = array([28.4344899,  76.8514270, 115.9701171, 147.6247860, 170.4726516, 186.1421232, 196.1767876, 201.9006600, 
	204.3632196, 204.3481327, 202.4115453, 198.9259190, 194.1176490, 188.0940199, 180.8592890, 172.3216961, 162.2942404, 
	150.4931119, 136.5386277, 119.9671428, 100.2586532,  77.0396762,  50.0666976,  20.4187276])

distortion = lambda t: 1. + 0.2 * sin(pi*t/1000.)

time = linspace(0,1500.,101.)
data = array([distortion(time)]).T * hstack([tcool, pcool, ql])
data = vstack([time, data.T])

header="""17
3.42
1.2850
0.413
0.422
0.4855
296.1
3856.63748813640
12 24
12*2
2*15.333293 20*16.099958 2*15.333293
93.8
%i
1"""%len(time)

savetxt('data.inp',data,fmt='%14.7f',header=header,comments='')

