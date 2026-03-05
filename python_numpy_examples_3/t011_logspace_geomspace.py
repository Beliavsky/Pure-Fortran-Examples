import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.logspace(0.0, 3.0, 4)
b = np.geomspace(1.0, 1000.0, 4)
print("a", np.round(a, 6).tolist())
print("b", np.round(b, 6).tolist())

