import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


def f(x):
    return x*x + 1

vf = np.vectorize(f)
x = np.array([0, 1, 2, 3])
y = vf(x)
print("x", x.tolist())
print("y", y.tolist())

