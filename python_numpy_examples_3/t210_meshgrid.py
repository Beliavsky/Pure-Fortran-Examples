import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([0.0, 0.5, 1.0])
y = np.array([-1.0, 1.0])
xx, yy = np.meshgrid(x, y, indexing="xy")
z = xx**2 + yy
print("xx", np.round(xx, 6).tolist())
print("yy", np.round(yy, 6).tolist())
print("z", np.round(z, 6).tolist())

