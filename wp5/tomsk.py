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

# df[5:6] 5 gave weird results

# I will input data or 11th row

x = 23,24,25,26,37,44,60,61,63
y = 11946,13731,14204,15627,19125,21983,22740,25018,19125
plt.plot(x,y)
#f = interp1d(x,y)
#f2 = interp1d(x, y, kind='cubic')

xnew = np.linspace(23, 64, num=41, endpoint=True)
#plt.plot(x, y, 'o', xnew, f(xnew), '-', xnew, f2(xnew), '--')
#plt.legend(['data', 'linear', 'cubic'], loc='best')
#plt.show()


f3 = interp1d(x, y, kind='cubic',fill_value='extrapolate')

#xnew = np.linspace(19, 25, num=41, endpoint=True)
plt.plot(x, y, 'o', xnew, f3(xnew),'--')
plt.legend(['data', 'linear', 'cubic'], loc='best')
plt.show()

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