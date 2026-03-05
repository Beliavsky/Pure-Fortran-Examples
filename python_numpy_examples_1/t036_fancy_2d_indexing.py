import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(16).reshape(4,4)
rows = np.array([0, 1, 3])
cols = np.array([1, 2, 0])
picked = a[rows, cols]
a2 = a.copy()
a2[rows, cols] = -1
print("a", a)
print("picked", picked)
print("a2", a2)
