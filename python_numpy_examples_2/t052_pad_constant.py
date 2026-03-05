import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([1, 2, 3], dtype=np.int64)
b = np.pad(a, pad_width=2, mode="constant", constant_values=0)
assert (b == np.array([0, 0, 1, 2, 3, 0, 0], dtype=np.int64)).all()
print("PASS")
