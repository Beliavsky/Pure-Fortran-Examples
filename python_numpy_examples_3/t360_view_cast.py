import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, -2.0, 3.5], dtype=np.float64)
u = x.view(np.uint64)
y = u.view(np.float64)
print("x", np.round(x, 6).tolist())
print("u", [int(v) for v in u.tolist()])
print("y", np.round(y, 6).tolist())

