import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3, 4])
a = (x >= 2)
b = (x % 2 == 0)
c = np.logical_and(a, b)
d = np.logical_or(a, b)
e = np.logical_xor(a, b)
print("x", x.tolist())
print("a", a.astype(np.int32).tolist())
print("b", b.astype(np.int32).tolist())
print("c", c.astype(np.int32).tolist())
print("d", d.astype(np.int32).tolist())
print("e", e.astype(np.int32).tolist())

