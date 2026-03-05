import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([0.0, 1.0, 4.0, 9.0], dtype=np.float64)
g = np.gradient(a)  # default spacing = 1
expected = np.array([1.0, 2.0, 4.0, 5.0], dtype=np.float64)
assert np.allclose(g, expected)
print("PASS")
