import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 3, 3, 7, 9])
v = np.array([0, 3, 4, 10])
l = np.searchsorted(x, v, side="left")
r = np.searchsorted(x, v, side="right")
print("x", x.tolist())
print("v", v.tolist())
print("l", l.tolist())
print("r", r.tolist())

