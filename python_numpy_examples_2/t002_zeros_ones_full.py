import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

z = np.zeros((2, 3), dtype=np.float64)
o = np.ones((2, 3), dtype=np.float64)
f = np.full((2, 3), 7.5, dtype=np.float64)
assert z.sum() == 0.0
assert o.sum() == 6.0
assert np.allclose(f, 7.5)
print("PASS")
