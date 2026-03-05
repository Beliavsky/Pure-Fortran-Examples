import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1.0, 2.0, 3.0, 10.0])
print("mean", x.mean())
print("var0", x.var(ddof=0))
print("var1", x.var(ddof=1))
print("std0", x.std(ddof=0))
print("std1", x.std(ddof=1))
