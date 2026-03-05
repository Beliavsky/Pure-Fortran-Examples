import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([0.0, np.pi / 2.0, np.pi], dtype=np.float64)
s = np.sin(a)
c = np.cos(a)
assert np.allclose(s, np.array([0.0, 1.0, 0.0]), atol=1e-15)
assert np.allclose(c, np.array([1.0, 0.0, -1.0]), atol=1e-15)
print("PASS")
