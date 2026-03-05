import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[1.0, 2.0], [3.0, 4.0]], dtype=np.float64)
b = np.array([[10.0, 20.0], [30.0, 40.0]], dtype=np.float64)
c = a @ b
expected = np.array([[70.0, 100.0], [150.0, 220.0]], dtype=np.float64)
assert np.allclose(c, expected)
print("PASS")
