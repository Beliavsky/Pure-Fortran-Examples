import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, 2.0, 3.0])
y = np.array([10.0, 20.0])
inn = np.inner(x, x)
out = np.outer(x, y)
print("x", x.tolist())
print("y", y.tolist())
print("inn", float(np.round(inn, 6)))
print("out", out.tolist())

