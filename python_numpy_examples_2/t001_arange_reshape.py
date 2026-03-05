import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(6, dtype=np.int64).reshape(2, 3)
b = a.copy()
assert a.shape == (2, 3)
assert a.dtype == np.int64
assert (a == np.array([[0, 1, 2], [3, 4, 5]], dtype=np.int64)).all()
assert (b == a).all()
print("PASS")
