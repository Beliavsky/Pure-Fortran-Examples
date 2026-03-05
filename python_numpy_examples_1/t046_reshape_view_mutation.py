import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(6)
b = a.reshape(2,3)
b[0,1] = 999
print("a", a)
print("b", b)
