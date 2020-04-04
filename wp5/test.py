# -*- coding: utf-8 -*-
"""
Created on Fri Apr  3 11:38:09 2020

@author: wb164718
"""

import numpy as np
import matplotlib.pyplot as plt


from scipy.interpolate import interp1d
x = np.linspace(0, 10, num=11, endpoint=True)
y = np.cos(-x**2/9.0)
f = interp1d(x, y)
f2 = interp1d(x, y, kind='cubic')

xnew = np.linspace(0, 10, num=41, endpoint=True)
plt.plot(x, y, 'o', xnew, f(xnew), '-', xnew, f2(xnew), '--')
plt.legend(['data', 'linear', 'cubic'], loc='best')
plt.show()


# Testing space
x = 19,20,21,22
y = 12041,14545,17713,18936
plt.plot(x,y)
f = interp1d(x,y)
f2 = interp1d(x, y, kind='cubic')

xnew = np.linspace(19, 22, num=41, endpoint=True)
plt.plot(x, y, 'o', xnew, f(xnew), '-', xnew, f2(xnew), '--')
plt.legend(['data', 'linear', 'cubic'], loc='best')
plt.show()


f3 = interp1d(x, y, kind='cubic',fill_value='extrapolate')

xnew = np.linspace(19, 25, num=41, endpoint=True)
plt.plot(x, y, 'o', xnew, f3(xnew),'--')
plt.legend(['data', 'linear', 'cubic'], loc='best')
plt.show()


# Playing with derivatibes
from scipy import interpolate
tck = interpolate.splrep(x, y, s=0)
yder = interpolate.splev(xnew, tck, der=1)
yder2 = interpolate.splev(xnew, tck, der=2)


plt.figure()
plt.plot(xnew, yder, xnew,'--')
plt.title('Derivative estimation from spline')
plt.show()