import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([2, 3, 4], dtype=np.int64)
b = a ** 3
c = np.mod(b, 5)
d = np.floor_divide(b, 5)
assert (b == np.array([8, 27, 64], dtype=np.int64)).all()
assert (c == np.array([3, 2, 4], dtype=np.int64)).all()
assert (d == np.array([1, 5, 12], dtype=np.int64)).all()
print("PASS")
