import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([10, 20, 30, 40, 50])
idx = np.array([4, 0, 3])
y = x[idx]
print("x", x.tolist())
print("idx", idx.tolist())
print("y", y.tolist())

