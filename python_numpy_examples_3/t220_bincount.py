import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([0, 1, 1, 3, 2, 1, 7, 7])
w = np.array([1.0, 1.0, 2.0, 1.0, 0.5, 1.5, 1.0, 3.0])
bc = np.bincount(x)
bw = np.bincount(x, weights=w)
print("x", x.tolist())
print("w", np.round(w, 6).tolist())
print("bc", bc.tolist())
print("bw", np.round(bw, 6).tolist())

