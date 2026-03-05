import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

a = np.array([-1.6, -1.1, 1.1, 1.6], dtype=np.float64)
r = np.round(a)
fl = np.floor(a)
ce = np.ceil(a)
assert np.allclose(r, np.array([-2.0, -1.0, 1.0, 2.0]))
assert np.allclose(fl, np.array([-2.0, -2.0, 1.0, 1.0]))
assert np.allclose(ce, np.array([-1.0, -1.0, 2.0, 2.0]))
print("PASS")
