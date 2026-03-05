import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


e = np.eye(3)
i = np.identity(3, dtype=np.int32)
v = np.array([10, 20, 30])
d = np.diag(v)
print("e", np.round(e, 6).tolist())
print("i", i.tolist())
print("d", d.tolist())

