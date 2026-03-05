import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


u = np.random.rand(5)
n = np.random.randn(5)
print("u", np.round(u, 6).tolist())
print("n", np.round(n, 6).tolist())

