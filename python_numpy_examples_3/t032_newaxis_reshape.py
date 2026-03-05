import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(1, 5)
a = x[:, np.newaxis]
b = x.reshape(-1, 1)
print("x", x.tolist())
print("a.shape", a.shape); print("a", a.tolist())
print("b.shape", b.shape); print("b", b.tolist())

