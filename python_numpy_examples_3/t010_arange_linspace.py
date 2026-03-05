import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(0, 10, 2)
b = np.linspace(0.0, 1.0, 6)
c = np.linspace(0.0, 1.0, 6, endpoint=False)
print("a.dtype", str(a.dtype)); print("a.shape", a.shape); print("a", a.tolist())
print("b.dtype", str(b.dtype)); print("b.shape", b.shape); print("b", np.round(b, 6).tolist())
print("c.dtype", str(c.dtype)); print("c.shape", c.shape); print("c", np.round(c, 6).tolist())

