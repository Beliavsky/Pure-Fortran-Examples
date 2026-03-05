import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([0.0, 0.5, 1.0, 2.0])
y = np.exp(x)
z = np.log(y + 1.0)
w = np.sqrt(z)
print("x", x)
print("y", y)
print("z", z)
print("w", w)
