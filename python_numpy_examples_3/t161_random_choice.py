import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([10, 20, 30, 40, 50])
c1 = np.random.choice(x, size=3, replace=False)
c2 = np.random.choice(x, size=6, replace=True)
print("x", x.tolist())
print("c1", c1.tolist())
print("c2", c2.tolist())

