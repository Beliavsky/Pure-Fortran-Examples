import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(6).reshape(2,3)
b = 10 + np.arange(4).reshape(2,2)
h = np.hstack([a, b])
v = np.vstack([a, a+100])
print("h", h)
print("v", v)
