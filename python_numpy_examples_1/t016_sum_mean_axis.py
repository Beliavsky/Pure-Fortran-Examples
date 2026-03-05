import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(12).reshape(3,4).astype(float)
print("a", a)
print("sum_all", a.sum())
print("sum_axis0", a.sum(axis=0))
print("sum_axis1", a.sum(axis=1))
print("mean_axis1", a.mean(axis=1))
