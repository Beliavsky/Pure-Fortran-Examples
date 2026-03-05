import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1.0, 2.0, 3.0])
b = np.broadcast_to(x, (4,3))
print("b.shape", b.shape)
print("b", b)
