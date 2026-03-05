import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 7).reshape(2, 3).astype(np.float64)
b = np.arange(1, 13).reshape(3, 4).astype(np.float64)
c = a @ b
d = np.dot(a, b)
print("a", a.tolist())
print("b", b.tolist())
print("c", c.tolist())
print("d", d.tolist())

