import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1,2,3,4], dtype=float)
print("x", x)
print("cumsum", np.cumsum(x))
print("cumprod", np.cumprod(x))
