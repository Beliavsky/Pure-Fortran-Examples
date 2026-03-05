import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(10).astype(float)
idx = np.array([2, 5, 7])
t = np.take(a, idx)
np.put(a, idx, np.array([100.0, 200.0, 300.0]))
a2 = a.copy()
a2 += 1.5
a3 = np.clip(a2, 0.0, 205.0)
print("take", t)
print("after_put", a)
print("a2", a2)
print("a3", a3)
