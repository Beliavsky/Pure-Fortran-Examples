import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(1, 13)
bC = a.reshape((3,4), order="C")
bF = a.reshape((3,4), order="F")
print("a", a)
print("bC", bC)
print("bF", bF)
