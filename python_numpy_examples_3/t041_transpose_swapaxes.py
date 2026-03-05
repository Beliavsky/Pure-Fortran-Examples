import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 13).reshape(3, 4)
t = a.T
s = np.swapaxes(a, 0, 1)
print("a", a.tolist())
print("t", t.tolist())
print("s", s.tolist())

