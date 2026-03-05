import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([-5, -1, 0, 1, 10], dtype=np.int64)
b = np.clip(a, 0, 3)
assert (b == np.array([0, 0, 0, 1, 3], dtype=np.int64)).all()
print("PASS")
