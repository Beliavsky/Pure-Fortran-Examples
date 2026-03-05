import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3])
y = np.array([10, 20])
z = x[:, None] + y[None, :]
print("x", x.tolist())
print("y", y.tolist())
print("z", z.tolist())

