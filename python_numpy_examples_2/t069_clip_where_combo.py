import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([-3, -1, 0, 1, 3], dtype=np.int64)
b = np.clip(a, -2, 2)
c = np.where(b < 0, 0, b)
assert (b == np.array([-2, -1, 0, 1, 2], dtype=np.int64)).all()
assert (c == np.array([0, 0, 0, 1, 2], dtype=np.int64)).all()
print("PASS")
