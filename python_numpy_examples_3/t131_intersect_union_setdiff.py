import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([1, 2, 2, 3, 7])
b = np.array([2, 3, 4, 4, 5])
i = np.intersect1d(a, b)
u = np.union1d(a, b)
d = np.setdiff1d(a, b)
print("a", a.tolist())
print("b", b.tolist())
print("i", i.tolist())
print("u", u.tolist())
print("d", d.tolist())

