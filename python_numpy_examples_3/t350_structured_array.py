import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


dt = np.dtype([("id", np.int32), ("x", np.float64)])
a = np.array([(1, 2.5), (2, -1.0), (3, 0.0)], dtype=dt)
print("a.dtype", str(a.dtype))
print("id", a["id"].tolist())
print("x", np.round(a["x"], 6).tolist())

