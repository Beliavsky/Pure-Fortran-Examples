import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, 2.0, 3.0, 4.0])
m = np.array([False, True, False, True])
mx = np.ma.array(x, mask=m)
print("x", x.tolist())
print("m", m.astype(np.int32).tolist())
print("mx_filled", mx.filled(-99.0).tolist())
print("mx_mean", float(np.round(mx.mean(), 6)))

