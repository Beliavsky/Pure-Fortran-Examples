import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3, 4], dtype=np.int64)
m1 = (a >= 2) & (a <= 3)
m2 = (a == 1) | (a == 4)
assert (m1 == np.array([False, True, True, False])).all()
assert (m2 == np.array([True, False, False, True])).all()
print("PASS")
