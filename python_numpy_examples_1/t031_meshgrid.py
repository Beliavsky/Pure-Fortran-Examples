import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([0.0, 1.0, 2.0])
y = np.array([10.0, 20.0])
xx, yy = np.meshgrid(x, y, indexing="xy")
zz = xx + yy
print("xx", xx)
print("yy", yy)
print("zz", zz)
