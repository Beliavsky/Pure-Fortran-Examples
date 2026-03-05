import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([3,1,2,3,2,2,1,3,0,0,0])
u = np.unique(x)
bc = np.bincount(x)
print("x", x)
print("unique", u)
print("bincount", bc)
