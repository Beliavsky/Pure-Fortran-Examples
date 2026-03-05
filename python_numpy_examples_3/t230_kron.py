import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([[1, 2], [3, 4]])
b = np.array([[0, 5], [6, 7]])
k = np.kron(a, b)
print("a", a.tolist())
print("b", b.tolist())
print("k", k.tolist())

