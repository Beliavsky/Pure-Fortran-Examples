import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([3, 1, 2, 1, 5])
s = np.sort(x)
a = np.argsort(x, kind="mergesort")
print("x", x.tolist())
print("s", s.tolist())
print("a", a.tolist())

