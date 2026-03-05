import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3], dtype=np.int64)
b = np.array([3, 2, 1], dtype=np.int64)
andv = np.bitwise_and(a, b)
orv = np.bitwise_or(a, b)
xorv = np.bitwise_xor(a, b)
sh = a << 1
assert (andv == np.array([1, 2, 1], dtype=np.int64)).all()
assert (orv == np.array([3, 2, 3], dtype=np.int64)).all()
assert (xorv == np.array([2, 0, 2], dtype=np.int64)).all()
assert (sh == np.array([2, 4, 6], dtype=np.int64)).all()
print("PASS")
