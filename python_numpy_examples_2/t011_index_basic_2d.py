import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(12, dtype=np.int64).reshape(3, 4)
x = a[1, 2]
row = a[2, :]
col = a[:, 1]
assert x == 6
assert (row == np.array([8, 9, 10, 11], dtype=np.int64)).all()
assert (col == np.array([1, 5, 9], dtype=np.int64)).all()
print("PASS")
