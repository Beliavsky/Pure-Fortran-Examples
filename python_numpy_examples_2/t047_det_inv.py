import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[4.0, 7.0], [2.0, 6.0]], dtype=np.float64)
det = np.linalg.det(a)
inv = np.linalg.inv(a)
iden = a @ inv
assert np.allclose(det, 10.0)
assert np.allclose(iden, np.eye(2), atol=1e-12)
print("PASS")
