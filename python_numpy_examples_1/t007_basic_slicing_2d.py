import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(20).reshape(4,5)
print("a", a)
print("a[1:3, 2:5]", a[1:3, 2:5])
print("a[:, ::2]", a[:, ::2])
