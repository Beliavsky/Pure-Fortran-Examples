import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(9).reshape(3,3).astype(float)
tr1 = np.trace(a)
tr2 = np.einsum("ii->", a)
print("a", a)
print("trace", tr1)
print("einsum_trace", tr2)
