import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1.0, 2.0, 3.0])
y = np.array([10.0, 20.0])
o = np.outer(x, y)
k = np.kron(x, y)
print("outer", o)
print("kron", k)
