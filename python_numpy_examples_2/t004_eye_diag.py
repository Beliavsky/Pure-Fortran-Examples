import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

i = np.eye(4, dtype=np.int64)
d = np.diag(np.array([10, 20, 30], dtype=np.int64))
assert (np.diag(i) == np.ones(4, dtype=np.int64)).all()
assert d.shape == (3, 3)
assert (np.diag(d) == np.array([10, 20, 30], dtype=np.int64)).all()
print("PASS")
