import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(1, 13)
a = x.reshape(3, 4)
b = a.ravel()
c = a.flatten()
print("x", x.tolist())
print("a.shape", a.shape); print("a", a.tolist())
print("b.shape", b.shape); print("b", b.tolist())
print("c.shape", c.shape); print("c", c.tolist())

