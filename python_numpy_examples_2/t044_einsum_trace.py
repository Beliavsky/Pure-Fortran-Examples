import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[1.0, 2.0], [3.0, 4.0]], dtype=np.float64)
tr1 = np.trace(a)
tr2 = np.einsum("ii->", a)
assert np.allclose(tr1, 5.0)
assert np.allclose(tr2, 5.0)
print("PASS")
