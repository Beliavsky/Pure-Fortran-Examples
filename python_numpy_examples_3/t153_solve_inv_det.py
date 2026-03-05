import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([[4.0, 1.0], [2.0, 3.0]])
b = np.array([1.0, 2.0])
x = np.linalg.solve(a, b)
ai = np.linalg.inv(a)
ad = np.linalg.det(a)
print("a", a.tolist())
print("b", b.tolist())
print("x", np.round(x, 6).tolist())
print("ai", np.round(ai, 6).tolist())
print("ad", float(np.round(ad, 6)))

