import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.arange(-5, 6)
y = np.where(x < 0, -x, x*x)
print("x", x)
print("y", y)
