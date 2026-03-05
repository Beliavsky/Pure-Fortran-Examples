import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([-1.0, 0.0, 0.5, 2.0])
a = np.exp(x)
b = np.log1p(np.abs(x))
c = np.sqrt(np.abs(x))
d = np.abs(x)
print("x", np.round(x, 6).tolist())
print("a", np.round(a, 6).tolist())
print("b", np.round(b, 6).tolist())
print("c", np.round(c, 6).tolist())
print("d", np.round(d, 6).tolist())

