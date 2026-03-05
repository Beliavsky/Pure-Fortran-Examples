import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([1, 2, 3])
b = np.array([10, 20, 30])
s = np.stack([a, b], axis=0)
v = np.vstack([a, b])
h = np.hstack([a, b])
print("s", s.tolist())
print("v", v.tolist())
print("h", h.tolist())

