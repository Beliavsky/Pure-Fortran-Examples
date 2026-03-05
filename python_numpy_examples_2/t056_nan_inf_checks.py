import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1.0, np.nan, np.inf, -np.inf, 0.0], dtype=np.float64)
isnan = np.isnan(a)
isfinite = np.isfinite(a)
assert (isnan == np.array([False, True, False, False, False])).all()
assert (isfinite == np.array([True, False, False, False, True])).all()
print("PASS")
