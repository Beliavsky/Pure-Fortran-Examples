import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1.0, 2.0, 4.0], dtype=np.float64)
b = np.log(a)
c = np.exp(b)
assert np.allclose(c, a)
print("PASS")
