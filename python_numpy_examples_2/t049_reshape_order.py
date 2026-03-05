import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(6, dtype=np.int64)
b = a.reshape((2, 3), order="C")
c = a.reshape((2, 3), order="F")
assert (b == np.array([[0, 1, 2], [3, 4, 5]], dtype=np.int64)).all()
assert (c == np.array([[0, 2, 4], [1, 3, 5]], dtype=np.int64)).all()
print("PASS")
