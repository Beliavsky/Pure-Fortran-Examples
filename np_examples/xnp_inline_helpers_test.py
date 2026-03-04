import numpy as np

a = np.array([1.0, 2.0, 2.0, 3.0])
b = np.array([5.0, 6.0])
c = np.array([4.0, 5.0])

z = np.where(a > 1.5, np.cumsum(a), np.cumprod(a))
u = np.unique(np.repeat(a, 1))
t = np.tile(b, 3)
d = np.diag(c)
e = np.diag(np.eye(3))

print("z:", z)
print("u:", u)
print("t:", t)
print("d:")
print(d)
print("e:", e)
