import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(24).reshape(2, 3, 4)
b = np.moveaxis(a, 0, -1)
c = np.moveaxis(a, -1, 0)
print("a.shape", a.shape); print("a0", a[0].tolist())
print("b.shape", b.shape); print("b0", b[...,0].tolist())
print("c.shape", c.shape); print("c0", c[0].tolist())

