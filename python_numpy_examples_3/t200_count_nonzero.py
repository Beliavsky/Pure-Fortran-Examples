import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([0, 1, 0, 3, 0, 5])
nz = np.count_nonzero(x)
idx = np.nonzero(x)[0]
print("x", x.tolist())
print("nz", int(nz))
print("idx", idx.tolist())

