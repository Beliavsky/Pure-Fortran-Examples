import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([3, 1, 3, 2, 1, 1], dtype=np.int64)
u, inv, cnt = np.unique(a, return_inverse=True, return_counts=True)
assert (u == np.array([1, 2, 3], dtype=np.int64)).all()
assert (cnt == np.array([3, 1, 2], dtype=np.int64)).all()
recon = u[inv]
assert (recon == a).all()
print("PASS")
