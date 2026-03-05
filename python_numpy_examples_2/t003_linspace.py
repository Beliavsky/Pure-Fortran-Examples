import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.linspace(0.0, 1.0, 6)
expected = np.array([0.0, 0.2, 0.4, 0.6, 0.8, 1.0])
assert np.allclose(x, expected)
print("PASS")
