import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([10, 20, 30, 40, 50])
y = np.take(x, [2, 0, 4])
z = x.copy()
np.put(z, [1, 3], [999, 888])
print("x", x.tolist())
print("y", y.tolist())
print("z", z.tolist())

