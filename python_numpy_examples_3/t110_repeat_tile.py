import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3])
r = np.repeat(x, 2)
t = np.tile(x, 3)
print("x", x.tolist())
print("r", r.tolist())
print("t", t.tolist())

