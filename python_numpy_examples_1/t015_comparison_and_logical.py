import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.array([1,2,3,4,5])
b = np.array([3,3,3,3,3])
m1 = a > b
m2 = (a >= 2) & (a <= 4)
m3 = (a == 1) | (a == 5)
print("m1", m1.astype(int))
print("m2", m2.astype(int))
print("m3", m3.astype(int))
