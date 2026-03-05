import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([1, 2, 3], dtype=np.int64)
y = np.array([10, 20], dtype=np.int64)
o = x[:, None] * y[None, :]
expected = np.array([[10, 20], [20, 40], [30, 60]], dtype=np.int64)
assert (o == expected).all()
print("PASS")
