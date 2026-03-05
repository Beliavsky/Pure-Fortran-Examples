import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 13).reshape(3, 4).astype(np.float64)
row_sums = a.sum(axis=1, keepdims=True)
b = a / row_sums
print("a", a.tolist())
print("row_sums", row_sums.tolist())
print("b", np.round(b, 6).tolist())

