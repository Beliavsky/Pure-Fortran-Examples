import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([-2.0, -1.0, 0.0, 1.0, 2.0], dtype=np.float64)
b = np.abs(a)
c = np.sqrt(b)
d = np.sign(a)
assert np.allclose(b, np.array([2, 1, 0, 1, 2], dtype=np.float64))
assert np.allclose(c, np.array([np.sqrt(2), 1, 0, 1, np.sqrt(2)], dtype=np.float64))
assert np.allclose(d, np.array([-1, -1, 0, 1, 1], dtype=np.float64))
print("PASS")
