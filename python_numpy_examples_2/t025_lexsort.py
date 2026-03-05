import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([1, 1, 0, 0], dtype=np.int64)
y = np.array([0, 1, 0, 1], dtype=np.int64)
idx = np.lexsort((y, x))  # sort by x, then y
sx = x[idx]
sy = y[idx]
assert (sx == np.array([0, 0, 1, 1], dtype=np.int64)).all()
assert (sy == np.array([0, 1, 0, 1], dtype=np.int64)).all()
print("PASS")
