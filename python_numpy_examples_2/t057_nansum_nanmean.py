import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1.0, np.nan, 3.0], dtype=np.float64)
s = np.nansum(a)
m = np.nanmean(a)
assert np.allclose(s, 4.0)
assert np.allclose(m, 2.0)
print("PASS")
