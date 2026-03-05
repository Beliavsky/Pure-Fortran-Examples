import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([1, 2, 3])
b = np.array([10, 20])
c = np.concatenate([a, b])
x = np.arange(6).reshape(2, 3)
y = np.arange(6, 12).reshape(2, 3)
z = np.concatenate([x, y], axis=0)
w = np.concatenate([x, y], axis=1)
print("c", c.tolist())
print("z", z.tolist())
print("w", w.tolist())

