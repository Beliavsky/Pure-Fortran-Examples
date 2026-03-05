import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[1, 2], [3, 4]], dtype=np.int64)
b = np.array([[0, 5], [6, 7]], dtype=np.int64)
k = np.kron(a, b)
expected = np.array([[0, 5, 0, 10],
                     [6, 7, 12, 14],
                     [0, 15, 0, 20],
                     [18, 21, 24, 28]], dtype=np.int64)
assert (k == expected).all()
print("PASS")
