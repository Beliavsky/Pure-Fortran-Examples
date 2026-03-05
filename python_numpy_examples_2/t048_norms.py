import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([3.0, 4.0], dtype=np.float64)
n2 = np.linalg.norm(x)
n1 = np.linalg.norm(x, ord=1)
assert np.allclose(n2, 5.0)
assert np.allclose(n1, 7.0)
print("PASS")
