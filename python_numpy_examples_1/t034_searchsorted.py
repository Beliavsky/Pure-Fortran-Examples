import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1, 3, 3, 7, 9])
v = np.array([0, 3, 4, 10])
i_left = np.searchsorted(x, v, side="left")
i_right = np.searchsorted(x, v, side="right")
print("x", x)
print("v", v)
print("left", i_left)
print("right", i_right)
