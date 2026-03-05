import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

def foo(a):
    b = a * a + 2.0 * a + 1.0
    return b

x = np.linspace(-2.0, 2.0, 5)
y = foo(x)
print("x", x)
print("y", y)
