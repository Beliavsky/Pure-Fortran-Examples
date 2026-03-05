import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([2, 1, 2, 1, 2])
b = np.array([10, 20, 30, 40, 50])
idx = np.lexsort((b, a))
print("a", a.tolist())
print("b", b.tolist())
print("idx", idx.tolist())
print("a_sorted", a[idx].tolist())
print("b_sorted", b[idx].tolist())

