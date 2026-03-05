import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3], dtype=np.int64)
b = np.array([4, 5, 6], dtype=np.int64)
v = np.vstack([a, b])
h = np.hstack([a, b])
c = np.concatenate([a, b])
assert v.shape == (2, 3)
assert (h == c).all()
assert (v[1] == b).all()
print("PASS")
