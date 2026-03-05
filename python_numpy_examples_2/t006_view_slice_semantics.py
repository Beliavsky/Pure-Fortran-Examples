import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(10, dtype=np.int64)
v = a[2:7]      # view
v[0] = 999
assert a[2] == 999
c = a[2:7].copy()
c[0] = 111
assert a[2] == 999
assert c[0] == 111
print("PASS")
