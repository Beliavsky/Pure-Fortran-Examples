import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(6, dtype=np.float64).reshape(2, 3)
s = a.sum(axis=1, keepdims=True)
assert s.shape == (2, 1)
assert np.allclose(s, np.array([[3.0], [12.0]]))
print("PASS")
