import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.array([[1.0, 2.0],
              [3.0, 4.0]])
w, v = np.linalg.eig(a)
u, s, vt = np.linalg.svd(a)
print("eigvals", w)
print("eigvecs", v)
print("svd_s", s)
print("svd_u", u)
print("svd_vt", vt)
