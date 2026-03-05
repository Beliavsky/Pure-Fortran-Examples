import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[3.0, 1.0], [1.0, 2.0]], dtype=np.float64)
b = np.array([9.0, 8.0], dtype=np.float64)
x = np.linalg.solve(a, b)
expected = np.array([2.0, 3.0], dtype=np.float64)
assert np.allclose(x, expected)
print("PASS")
