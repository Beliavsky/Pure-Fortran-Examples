import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

m = np.array([True, False, True], dtype=bool)
a = m.astype(np.int64)
assert (a == np.array([1, 0, 1], dtype=np.int64)).all()
print("PASS")
