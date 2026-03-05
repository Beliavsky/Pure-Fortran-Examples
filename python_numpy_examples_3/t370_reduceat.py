import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3, 4, 5, 6])
idx = np.array([0, 2, 5])
y = np.add.reduceat(x, idx)
print("x", x.tolist())
print("idx", idx.tolist())
print("y", y.tolist())

