import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([5, 2, 9, 1, 5, 6])
xs = np.sort(x)
ix = np.argsort(x)
print("x", x)
print("sorted", xs)
print("argsort", ix)
print("x[argsort]", x[ix])
