import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(12, dtype=np.float64).reshape(3, 4)
s0 = a.sum(axis=0)
s1 = a.sum(axis=1)
m0 = a.mean(axis=0)
assert np.allclose(s0, np.array([12, 15, 18, 21], dtype=np.float64))
assert np.allclose(s1, np.array([6, 22, 38], dtype=np.float64))
assert np.allclose(m0, s0 / 3.0)
print("PASS")
