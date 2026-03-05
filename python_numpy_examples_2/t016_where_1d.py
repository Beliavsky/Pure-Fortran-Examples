import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([-2, -1, 0, 1, 2], dtype=np.int64)
b = np.where(a >= 0, a, 0)
assert (b == np.array([0, 0, 0, 1, 2], dtype=np.int64)).all()
print("PASS")
