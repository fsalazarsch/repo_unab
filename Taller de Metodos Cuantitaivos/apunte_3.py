# -*- coding: utf-8 -*-
"""Untitled0.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1dEwB0pImcnQB8PxRt2pb509e6ScU7QG2
"""

import pandas as np
import statsmodels.formula.api as sfm

y = [15, 20, 15, 15, 20]
y = np.array(y)

X1 = [90, 100, 80, 70,100]
X1 = np.array(X1)
X2 = [3, 3.2, 3.2, 3,4]
X2 = np.array(X2)

bdd = {}
bdd["y"] = y
bdd["X1"] = X1
bdd["X2"] = X2

lm = sfm.ols(formula="y~X1+X2",data=bdd).fit()

lm.summary()
#ols = "mínimos cuadrados ordinarios"
lm.params #parametros de la ecuación