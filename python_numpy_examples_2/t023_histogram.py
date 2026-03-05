import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([0.1, 0.2, 0.2, 0.9, 1.1, 1.9], dtype=np.float64)
h, edges = np.histogram(x, bins=np.array([0.0, 0.5, 1.0, 2.0]))
assert (h == np.array([3, 1, 2], dtype=np.int64)).all()
assert np.allclose(edges, np.array([0.0, 0.5, 1.0, 2.0]))
print("PASS")
