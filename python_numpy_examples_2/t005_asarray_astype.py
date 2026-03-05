import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

lst = [1, 2, 3, 4]
a = np.asarray(lst)
b = a.astype(np.float64)
assert a.dtype == np.int64 or a.dtype == np.int32
assert b.dtype == np.float64
assert np.allclose(b, np.array([1.0, 2.0, 3.0, 4.0]))
print("PASS")
