import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.zeros((2, 3))
b = np.ones((2, 3), dtype=np.int32)
c = np.full((2, 3), 7.5)
d = np.empty((2, 3))
print("a", np.round(a, 6).tolist())
print("b", b.tolist())
print("c", np.round(c, 6).tolist())
print("d.shape", d.shape); print("d.dtype", str(d.dtype))

