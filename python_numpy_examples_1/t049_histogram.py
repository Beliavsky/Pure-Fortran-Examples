import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([0.1, 0.2, 0.9, 1.1, 1.2, 1.9, 2.0, 2.1])
h, edges = np.histogram(x, bins=[0.0, 1.0, 2.0, 3.0])
print("x", x)
print("h", h)
print("edges", edges)
