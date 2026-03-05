import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

rng = np.random.default_rng(0)
a = rng.normal(size=(2, 3))
b = rng.integers(low=0, high=10, size=5, dtype=np.int64)
assert a.shape == (2, 3)
assert b.shape == (5,)
assert b.dtype == np.int64
# Do not hardcode exact floats; just basic sanity checks
assert np.isfinite(a).all()
assert ((b >= 0) & (b < 10)).all()
print("PASS")
