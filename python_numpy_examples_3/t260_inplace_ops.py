import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([1, 2, 3], dtype=np.int32)
b = np.array([0.5, 1.5, -2.0], dtype=np.float64)
c = a.astype(np.float64)
c += b
d = c.copy()
d *= 2.0
print("a", a.tolist())
print("b", np.round(b, 6).tolist())
print("c", np.round(c, 6).tolist())
print("d", np.round(d, 6).tolist())

