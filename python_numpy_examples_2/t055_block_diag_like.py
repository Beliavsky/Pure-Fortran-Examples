import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([[1, 2], [3, 4]], dtype=np.int64)
b = np.array([[5]], dtype=np.int64)
z = np.zeros((3, 3), dtype=np.int64)
z[:2, :2] = a
z[2, 2] = b[0, 0]
expected = np.array([[1, 2, 0],
                     [3, 4, 0],
                     [0, 0, 5]], dtype=np.int64)
assert (z == expected).all()
print("PASS")
