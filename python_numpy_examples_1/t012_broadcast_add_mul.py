import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(6).reshape(3,2).astype(float)
b = np.array([10.0, 100.0])
c = a + b
d = a * b
print("a", a)
print("b", b)
print("c", c)
print("d", d)
