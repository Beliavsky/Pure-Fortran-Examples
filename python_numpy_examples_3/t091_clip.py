import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([-2.0, -1.0, 0.0, 1.0, 2.0, 10.0])
c = np.clip(x, -1.0, 2.0)
mn = np.minimum(x, 1.5)
mx = np.maximum(x, 0.5)
print("x", np.round(x, 6).tolist())
print("c", np.round(c, 6).tolist())
print("mn", np.round(mn, 6).tolist())
print("mx", np.round(mx, 6).tolist())

