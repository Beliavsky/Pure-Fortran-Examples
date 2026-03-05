import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[1, 2, 3],
              [4, 5, 6]], dtype=np.int64)
v = np.array([2, 2, 2], dtype=np.int64)
m = a > v
expected = np.array([[False, False, True],
                     [True, True, True]])
assert (m == expected).all()
print("PASS")
