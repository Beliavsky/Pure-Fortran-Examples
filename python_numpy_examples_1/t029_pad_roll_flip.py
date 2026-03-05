import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1,2,3,4,5])
p = np.pad(x, (2,3), mode="constant", constant_values=0)
r = np.roll(x, 2)
f = np.flip(x)
print("x", x)
print("pad", p)
print("roll", r)
print("flip", f)
