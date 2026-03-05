import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, -2.0, 3.0, -4.0, 5.0])
m = (x > 0)
s = x.sum(where=m)
print("x", x.tolist())
print("m", m.astype(np.int32).tolist())
print("s", float(np.round(s, 6)))

