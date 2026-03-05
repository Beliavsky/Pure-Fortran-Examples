import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.array([[3, 1, 9],
              [2, 7, 0]], dtype=float)
print("a", a)
print("min", a.min())
print("max", a.max())
print("argmin", np.argmin(a))
print("argmax", np.argmax(a))
print("min_axis1", a.min(axis=1))
print("argmax_axis0", np.argmax(a, axis=0))
