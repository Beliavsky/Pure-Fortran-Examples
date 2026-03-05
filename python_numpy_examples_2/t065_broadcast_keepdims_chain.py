import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.arange(12, dtype=np.float64).reshape(3, 4)
m = a.mean(axis=0, keepdims=True)     # (1,4)
b = a - m
# column means of b should be approx 0
cm = b.mean(axis=0)
assert np.allclose(cm, np.zeros(4), atol=1e-15)
print("PASS")
