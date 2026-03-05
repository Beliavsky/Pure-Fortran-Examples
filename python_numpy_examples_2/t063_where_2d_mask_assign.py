import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(9, dtype=np.int64).reshape(3, 3)
m = a % 2 == 0
b = a.copy()
b[m] = -1
expected = np.array([[-1, 1, -1],
                     [3, -1, 5],
                     [-1, 7, -1]], dtype=np.int64)
assert (b == expected).all()
print("PASS")
