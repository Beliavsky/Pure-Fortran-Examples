import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([0.0, 1.0, 4.0, 9.0, 16.0])
d = np.diff(x)
g = np.gradient(x)
print("x", x)
print("diff", d)
print("gradient", g)
