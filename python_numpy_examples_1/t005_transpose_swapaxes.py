import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(24).reshape(2,3,4)
b = a.transpose(2,1,0)
c = np.swapaxes(a, 0, 2)
print("a.shape", a.shape)
print("b.shape", b.shape)
print("c.shape", c.shape)
print("b[0,0,0]", b[0,0,0])
print("c[0,0,0]", c[0,0,0])
