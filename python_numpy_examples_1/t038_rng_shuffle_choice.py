import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

rng = np.random.default_rng(123)
x = np.arange(10)
rng.shuffle(x)
c = rng.choice(np.arange(5), size=8, replace=True)
print("shuffled", x)
print("choice", c)
