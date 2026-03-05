import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([[3, 1, 2], [9, 0, 5]])
s0 = np.sort(a, axis=0)
s1 = np.sort(a, axis=1)
print("a", a.tolist())
print("s0", s0.tolist())
print("s1", s1.tolist())

