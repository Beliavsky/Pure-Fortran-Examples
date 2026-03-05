import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(24, dtype=np.int64).reshape(2, 3, 4)
t = np.transpose(a, (2, 1, 0))
s = np.swapaxes(a, 0, 2)
assert t.shape == (4, 3, 2)
assert s.shape == (4, 3, 2)
assert (t[:, :, 0] == a[0].T).all()
print("PASS")
