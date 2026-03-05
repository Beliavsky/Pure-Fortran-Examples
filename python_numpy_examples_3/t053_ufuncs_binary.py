import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, 2.0, 3.0])
y = np.array([0.5, 2.5, -1.0])
a = np.add(x, y)
b = np.multiply(x, y)
c = np.maximum(x, y)
d = np.power(x, 2.0)
print("x", np.round(x, 6).tolist())
print("y", np.round(y, 6).tolist())
print("a", np.round(a, 6).tolist())
print("b", np.round(b, 6).tolist())
print("c", np.round(c, 6).tolist())
print("d", np.round(d, 6).tolist())

