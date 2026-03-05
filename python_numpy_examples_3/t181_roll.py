import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(10)
r1 = np.roll(x, 3)
a = np.arange(12).reshape(3, 4)
r2 = np.roll(a, shift=1, axis=0)
r3 = np.roll(a, shift=-2, axis=1)
print("x", x.tolist())
print("r1", r1.tolist())
print("a", a.tolist())
print("r2", r2.tolist())
print("r3", r3.tolist())

