import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1,2,3])
print("repeat2", np.repeat(x, 2))
print("tile3", np.tile(x, 3))
a = np.arange(6).reshape(2,3)
print("tile2x1", np.tile(a, (2,1)))
