import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, 2.0, 3.0])
y = np.array([10.0, 20.0, 30.0])
d = np.einsum("i,i->", x, y)
o = np.einsum("i,j->ij", x, y)
print("x", x.tolist())
print("y", y.tolist())
print("d", float(np.round(d, 6)))
print("o", o.tolist())

