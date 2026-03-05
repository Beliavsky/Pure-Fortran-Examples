import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

rng = np.random.default_rng(1)
x = np.array([10, 20, 30, 40], dtype=np.int64)
p = rng.permutation(x)
c = rng.choice(x, size=3, replace=True)
assert p.shape == (4,)
assert c.shape == (3,)
assert set(p.tolist()) == set(x.tolist())
print("PASS")
