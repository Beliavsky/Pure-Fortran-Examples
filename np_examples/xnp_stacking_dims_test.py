import numpy as np

v1 = np.array([1, 2, 3])
v2 = np.array([10, 20, 30])

m1 = np.array([[1, 2], [3, 4]])
m2 = np.array([[10, 20], [30, 40]])

h1 = np.hstack((v1, v2))
v1s = np.vstack((v1, v2))
c1 = np.column_stack((v1, v2))

h2 = np.hstack((m1, m2))
v2s = np.vstack((m1, m2))

x = np.expand_dims(v1, 1)
y = np.squeeze(x)

print("h1:", h1)
print("v1s:")
print(v1s)
print("c1:")
print(c1)
print("h2:")
print(h2)
print("v2s:")
print(v2s)
print("expand_dims:")
print(x)
print("squeeze:", y)
