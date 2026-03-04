import numpy as np

a = np.array([0, 2, -3, 4], dtype=float)
b = np.array([1, 1, 1, 1], dtype=float)
mx = np.maximum(a, b)
mn = np.minimum(a, b)
print("maximum:", mx)
print("minimum:", mn)

m = np.array([[1.0, 0.0, 3.0], [0.0, 0.0, 5.0]])
print("all:", np.all(m))
print("any:", np.any(m))
print("all axis=0:", np.all(m, axis=0))
print("any axis=1:", np.any(m, axis=1))
print("all axis=1 keepdims:")
print(np.all(m, axis=1, keepdims=True))
print("any axis=0 keepdims:")
print(np.any(m, axis=0, keepdims=True))
