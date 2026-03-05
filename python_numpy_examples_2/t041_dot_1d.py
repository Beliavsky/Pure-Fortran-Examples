import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([1.0, 2.0, 3.0], dtype=np.float64)
y = np.array([10.0, 20.0, 30.0], dtype=np.float64)
d = np.dot(x, y)
assert np.allclose(d, 140.0)
print("PASS")
