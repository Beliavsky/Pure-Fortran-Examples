import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(12, dtype=np.int64).reshape(3, 4)
i = np.ravel_multi_index((2, 1), a.shape)  # row=2,col=1
rc = np.unravel_index(i, a.shape)
assert i == 9
assert rc == (2, 1)
print("PASS")
