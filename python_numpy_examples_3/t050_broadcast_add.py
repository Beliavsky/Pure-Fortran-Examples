import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 7).reshape(2, 3)
b = np.array([10, 20, 30])
c = a + b
print("a", a.tolist())
print("b", b.tolist())
print("c", c.tolist())

