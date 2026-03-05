import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([5, 1, 3, 9, 2], dtype=np.int64)
mn = a.min()
mx = a.max()
imin = a.argmin()
imax = a.argmax()
assert mn == 1
assert mx == 9
assert imin == 1
assert imax == 3
print("PASS")
