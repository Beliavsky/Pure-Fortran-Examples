import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.arange(1, 6).astype(float)
s = 0.0
for i in range(x.size):
    s += (i+1) * x[i]
print("x", x)
print("s", s)
