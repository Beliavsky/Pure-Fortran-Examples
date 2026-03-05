import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(12).reshape(3,4)
r1 = a.ravel()
r2 = a.flatten()
a[0,0] = 999
print("a", a)
print("ravel", r1)
print("flatten", r2)
