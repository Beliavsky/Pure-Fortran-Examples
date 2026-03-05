import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([0, 2, 0, 3, 4, 0], dtype=np.int64)
nz = np.nonzero(a)
aw = np.argwhere(a)
assert (nz[0] == np.array([1, 3, 4], dtype=np.int64)).all()
assert (aw.ravel() == np.array([1, 3, 4], dtype=np.int64)).all()
print("PASS")
