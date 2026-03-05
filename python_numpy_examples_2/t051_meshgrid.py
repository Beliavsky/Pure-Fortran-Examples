import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([1, 2, 3], dtype=np.int64)
y = np.array([10, 20], dtype=np.int64)
xx, yy = np.meshgrid(x, y, indexing="xy")
assert xx.shape == (2, 3)
assert yy.shape == (2, 3)
assert (xx[0] == x).all()
assert (yy[:, 0] == y).all()
print("PASS")
