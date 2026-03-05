import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3, 4])
y = x.astype(np.float64)
z = y.astype(np.int32)
print("x.dtype", str(x.dtype)); print("x", x.tolist())
print("y.dtype", str(y.dtype)); print("y", np.round(y, 6).tolist())
print("z.dtype", str(z.dtype)); print("z", z.tolist())

