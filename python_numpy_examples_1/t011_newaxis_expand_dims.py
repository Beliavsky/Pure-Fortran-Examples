import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.arange(4)
a = x[:, None]  # shape (4,1)
b = x[None, :]  # shape (1,4)
c = a + b
print("a.shape", a.shape)
print("b.shape", b.shape)
print("c", c)
