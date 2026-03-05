import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(10)
a = np.split(x, [3, 7])
b = np.array_split(x, 4)
print("x", x.tolist())
print("a0", a[0].tolist()); print("a1", a[1].tolist()); print("a2", a[2].tolist())
print("b0", b[0].tolist()); print("b1", b[1].tolist()); print("b2", b[2].tolist()); print("b3", b[3].tolist())

