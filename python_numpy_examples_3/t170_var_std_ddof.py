import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, 2.0, 3.0, 4.0])
v0 = x.var()
v1 = x.var(ddof=1)
s0 = x.std()
s1 = x.std(ddof=1)
print("x", x.tolist())
print("v0", float(np.round(v0, 6)))
print("v1", float(np.round(v1, 6)))
print("s0", float(np.round(s0, 6)))
print("s1", float(np.round(s1, 6)))

