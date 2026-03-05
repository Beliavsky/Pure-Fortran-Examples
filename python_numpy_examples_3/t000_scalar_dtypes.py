import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array(3)
b = np.array(3.5)
c = np.array(True)
print("a.dtype", str(a.dtype)); print("a.shape", a.shape); print("a", int(a))
print("b.dtype", str(b.dtype)); print("b.shape", b.shape); print("b", float(np.round(b, 6)))
print("c.dtype", str(c.dtype)); print("c.shape", c.shape); print("c", bool(c))

