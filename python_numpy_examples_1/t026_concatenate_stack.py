import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(6).reshape(2,3)
b = 100 + np.arange(6).reshape(2,3)
c0 = np.concatenate([a,b], axis=0)
c1 = np.concatenate([a,b], axis=1)
s0 = np.stack([a,b], axis=0)
s1 = np.stack([a,b], axis=2)
print("c0", c0)
print("c1", c1)
print("s0.shape", s0.shape)
print("s1.shape", s1.shape)
