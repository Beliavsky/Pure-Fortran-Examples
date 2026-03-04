import numpy as np

a = np.array([1.0, 10.0, 100.0, 0.25])
print("log10:", np.log10(a))
print("log2:", np.log2(a))

x = np.array([1.0, 0.0, -1.0])
y = 1.0 / x
z = y - y
print("isfinite:", np.isfinite(y))
print("isinf:", np.isinf(y))
print("isnan:", np.isnan(z))
