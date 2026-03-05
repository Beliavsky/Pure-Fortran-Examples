import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([0.1, 0.2, 0.2, 0.8, 1.1, 1.5, 1.9])
h, edges = np.histogram(x, bins=[0.0, 0.5, 1.0, 2.0])
print("x", np.round(x, 6).tolist())
print("h", h.tolist())
print("edges", np.round(edges, 6).tolist())

