import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[1.0, 2.0, 3.0],
              [4.0, 5.0, 6.0]], dtype=np.float64)
v = np.array([1.0, 2.0, 3.0], dtype=np.float64)
b = a / v
expected = np.array([[1.0, 1.0, 1.0],
                     [4.0, 2.5, 2.0]], dtype=np.float64)
assert np.allclose(b, expected)
print("PASS")
