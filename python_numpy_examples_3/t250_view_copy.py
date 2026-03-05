import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(10)
v = a[2:6]     # view
c = a[2:6].copy()
v[0] = 999
c[1] = 888
print("a", a.tolist())
print("v", v.tolist())
print("c", c.tolist())

