import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

rng = np.random.default_rng(123)
x = rng.normal(0.0, 1.0, size=5)
y = rng.uniform(-1.0, 1.0, size=(2,3))
z = rng.integers(0, 10, size=8)
print("x", x)
print("y", y)
print("z", z)
