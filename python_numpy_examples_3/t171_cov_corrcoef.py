import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, 2.0, 3.0, 4.0])
y = np.array([2.0, 1.0, 2.0, 3.0])
c = np.cov(x, y, ddof=1)
r = np.corrcoef(x, y)
print("x", x.tolist())
print("y", y.tolist())
print("c", np.round(c, 6).tolist())
print("r", np.round(r, 6).tolist())

