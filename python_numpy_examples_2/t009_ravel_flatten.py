import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(6, dtype=np.int64).reshape(2, 3)
r = a.ravel()
f = a.flatten()
r[0] = 999
assert a[0, 0] == 999  # ravel is view when possible
f[1] = 888
assert a[0, 1] != 888  # flatten is copy
print("PASS")
