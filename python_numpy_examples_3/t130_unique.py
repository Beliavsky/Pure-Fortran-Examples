import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([3, 1, 2, 1, 5, 3, 3])
u = np.unique(x)
uu, cnt = np.unique(x, return_counts=True)
print("x", x.tolist())
print("u", u.tolist())
print("uu", uu.tolist())
print("cnt", cnt.tolist())

