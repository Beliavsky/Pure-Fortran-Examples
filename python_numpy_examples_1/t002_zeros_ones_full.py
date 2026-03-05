import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.zeros((2,3))
b = np.ones((2,3), dtype=np.int64)
c = np.full((2,3), 7.5)
print("a", a)
print("b", b, b.dtype)
print("c", c)
