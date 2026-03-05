import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.array([[4.0, 1.0],
              [2.0, 3.0]])
b = np.array([1.0, 2.0])
x = np.linalg.solve(a, b)
det = np.linalg.det(a)
inv = np.linalg.inv(a)
print("a", a)
print("b", b)
print("x", x)
print("det", det)
print("inv", inv)
