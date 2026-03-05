import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([[1, 2], [3, 4]])
r0 = np.repeat(a, 2, axis=0)
r1 = np.repeat(a, 3, axis=1)
print("a", a.tolist())
print("r0", r0.tolist())
print("r1", r1.tolist())

