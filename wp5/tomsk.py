# -*- coding: utf-8 -*-
"""
Created on Mon Apr 6 9:59:34 2020

@author: wb164718
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from scipy.interpolate import interp1d


# Get ze Tomsk data generated in rosstat2a.R

df = pd.read_csv('C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp5/ndf69.csv')

df

df[5:6]

# I will input data or 5th row

x = 27,28,29,30,36,43,45,49,57
y = 24628,29046,25697,27637,26820,31631,27984,30097,26820
plt.plot(x,y)
#f = interp1d(x,y)
#f2 = interp1d(x, y, kind='cubic')

xnew = np.linspace(27, 64, num=37, endpoint=True)
#plt.plot(x, y, 'o', xnew, f(xnew), '-', xnew, f2(xnew), '--')
#plt.legend(['data', 'linear', 'cubic'], loc='best')
#plt.show()


f3 = interp1d(x, y, kind='cubic',fill_value='extrapolate')

#xnew = np.linspace(19, 25, num=41, endpoint=True)
plt.plot(x, y, 'o', xnew, f3(xnew),'--')
plt.legend(['data', 'linear', 'cubic'], loc='best')
plt.show()
f3
xnew
f3(xnew)
# Playing with derivatibes
from scipy import interpolate
tck = interpolate.splrep(x, y, s=0)
yder = interpolate.splev(xnew, tck, der=1)
yder2 = interpolate.splev(xnew, tck, der=2)


plt.figure()
plt.plot(xnew, yder, xnew,'--')
plt.title('Derivative estimation from spline')
plt.show()