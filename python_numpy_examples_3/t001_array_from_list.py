import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3, 4])
y = np.array([[1.0, 2.0], [3.0, 4.5]])
print("x.dtype", str(x.dtype)); print("x.shape", x.shape); print("x", x.tolist())
print("y.dtype", str(y.dtype)); print("y.shape", y.shape); print("y", np.round(y, 6).tolist())

