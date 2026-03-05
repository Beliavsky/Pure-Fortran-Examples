import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([3.0, -1.0, 2.0, 2.0, 9.0])
print("x", np.round(x, 6).tolist())
print("min", float(np.round(x.min(), 6)))
print("max", float(np.round(x.max(), 6)))
print("argmin", int(x.argmin()))
print("argmax", int(x.argmax()))

