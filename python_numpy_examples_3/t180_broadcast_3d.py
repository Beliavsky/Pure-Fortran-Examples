import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(6).reshape(2, 3, 1)
b = np.arange(12).reshape(1, 3, 4)
c = a + b
print("a.shape", a.shape); print("b.shape", b.shape); print("c.shape", c.shape)
print("c0", c[0].tolist())

