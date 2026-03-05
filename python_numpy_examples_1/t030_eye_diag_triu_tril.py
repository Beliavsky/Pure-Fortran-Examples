import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(1, 10).reshape(3,3).astype(float)
e = np.eye(3)
d = np.diag(np.array([1.0, 2.0, 3.0]))
du = np.diag(a, k=1)
tu = np.triu(a)
tl = np.tril(a)
print("a", a)
print("eye", e)
print("diag", d)
print("diag_k1", du)
print("triu", tu)
print("tril", tl)
