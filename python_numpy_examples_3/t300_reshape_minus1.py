import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(24)
a = x.reshape(2, -1, 4)
print("x.shape", x.shape)
print("a.shape", a.shape)
print("a0", a[0].tolist())

