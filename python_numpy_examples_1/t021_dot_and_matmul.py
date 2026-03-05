import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(6).reshape(2,3).astype(float)
b = np.arange(12).reshape(3,4).astype(float)
c = a.dot(b)
d = a @ b
print("a", a)
print("b", b)
print("c", c)
print("d", d)
