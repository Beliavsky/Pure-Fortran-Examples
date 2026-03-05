import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(10)
m = (x % 3 == 0)
y = x[m]
z = x.copy()
z[m] = -1
print("x", x.tolist())
print("m", m.astype(np.int32).tolist())
print("y", y.tolist())
print("z", z.tolist())

