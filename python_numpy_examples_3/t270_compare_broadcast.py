import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 7).reshape(2, 3)
b = np.array([2, 4, 6])
m = (a <= b)
print("a", a.tolist())
print("b", b.tolist())
print("m", m.astype(np.int32).tolist())

