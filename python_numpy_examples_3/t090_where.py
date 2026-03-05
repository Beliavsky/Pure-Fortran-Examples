import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(-3, 4).astype(np.int32)
y = np.where(x < 0, -x, x)
print("x", x.tolist())
print("y", y.tolist())

