import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([True, True, False, True])
y = np.array([1, 2, 3, 4])
m = (y >= 2)
print("x", x.astype(np.int32).tolist())
print("all_x", bool(np.all(x)))
print("any_x", bool(np.any(x)))
print("m", m.astype(np.int32).tolist())
print("all_m", bool(np.all(m)))
print("any_m", bool(np.any(m)))

