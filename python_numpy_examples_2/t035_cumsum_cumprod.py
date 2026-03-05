import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3, 4], dtype=np.int64)
cs = np.cumsum(a)
cp = np.cumprod(a)
assert (cs == np.array([1, 3, 6, 10], dtype=np.int64)).all()
assert (cp == np.array([1, 2, 6, 24], dtype=np.int64)).all()
print("PASS")
