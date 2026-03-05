import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([10.0, 11.5, 9.0, 9.0, 12.0])
d1 = np.diff(x)
d2 = np.diff(x, n=2)
d3 = np.diff(x, prepend=x[0])
print("x", np.round(x, 6).tolist())
print("d1", np.round(d1, 6).tolist())
print("d2", np.round(d2, 6).tolist())
print("d3", np.round(d3, 6).tolist())

