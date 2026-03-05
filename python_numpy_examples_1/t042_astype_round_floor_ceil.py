import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([1.2, 2.5, -3.7, 4.0])
print("round", np.round(x, 0))
print("floor", np.floor(x))
print("ceil", np.ceil(x))
print("astype_int", x.astype(np.int64))
