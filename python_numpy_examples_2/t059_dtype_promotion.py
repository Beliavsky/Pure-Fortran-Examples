import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3], dtype=np.int32)
b = np.array([1.5, 2.5, 3.5], dtype=np.float64)
c = a + b
assert c.dtype == np.float64
assert np.allclose(c, np.array([2.5, 4.5, 6.5], dtype=np.float64))
print("PASS")
