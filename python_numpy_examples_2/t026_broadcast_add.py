import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(6, dtype=np.int64).reshape(2, 3)
v = np.array([10, 20, 30], dtype=np.int64)
b = a + v
expected = np.array([[10, 21, 32], [13, 24, 35]], dtype=np.int64)
assert (b == expected).all()
print("PASS")
