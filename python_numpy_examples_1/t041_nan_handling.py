import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1.0, np.nan, 3.0, np.nan, 5.0])
m = np.isnan(x)
y = np.where(m, 0.0, x)
print("x", x)
print("isnan", m.astype(int))
print("y", y)
print("nansum", np.nansum(x))
