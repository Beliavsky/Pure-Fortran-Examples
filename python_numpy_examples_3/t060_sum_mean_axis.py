import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 13).reshape(3, 4).astype(np.float64)
s0 = a.sum(axis=0)
s1 = a.sum(axis=1)
m0 = a.mean(axis=0)
m1 = a.mean(axis=1, keepdims=True)
print("a", a.tolist())
print("s0", s0.tolist())
print("s1", s1.tolist())
print("m0", np.round(m0, 6).tolist())
print("m1.shape", m1.shape); print("m1", np.round(m1, 6).tolist())

