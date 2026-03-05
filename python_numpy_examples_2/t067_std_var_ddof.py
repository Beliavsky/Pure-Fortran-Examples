import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float64)
v0 = a.var(ddof=0)
v1 = a.var(ddof=1)
s0 = a.std(ddof=0)
s1 = a.std(ddof=1)
assert np.allclose(v0, 1.25)
assert np.allclose(v1, 1.6666666666666667)
assert np.allclose(s0 * s0, v0)
assert np.allclose(s1 * s1, v1)
print("PASS")
