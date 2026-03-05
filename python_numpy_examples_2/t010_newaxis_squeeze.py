import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3], dtype=np.int64)
b = a[:, np.newaxis]      # (3,1)
c = np.squeeze(b)         # (3,)
assert b.shape == (3, 1)
assert c.shape == (3,)
assert (c == a).all()
print("PASS")
