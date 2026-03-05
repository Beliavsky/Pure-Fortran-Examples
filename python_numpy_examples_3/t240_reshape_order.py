import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(1, 7)
a = x.reshape((2, 3), order="C")
b = x.reshape((2, 3), order="F")
print("x", x.tolist())
print("a", a.tolist())
print("b", b.tolist())

