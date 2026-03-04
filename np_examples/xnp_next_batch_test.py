import numpy as np

a = np.array([1.0, 3.0, 6.0, 10.0])
m = np.array([[1.0, 2.0, 4.0], [3.0, 5.0, 9.0]])
mask = np.array([[0, 2, 0], [3, 4, 0]])

print("diff1:", np.diff(a))
print("diff2 axis=1:")
print(np.diff(m, axis=1))
print("count_nonzero axis=1:", np.count_nonzero(mask, axis=1))
print("count_nonzero axis=0 keepdims:")
print(np.count_nonzero(mask, axis=0, keepdims=True))
print("clip max only:", np.clip(a, None, 6.0))
print("clip min only:", np.clip(a, 2.0, None))
print("full:", np.full(4, 2.5))
print("full in concat:", np.concatenate((np.full(2, 0.0), np.full(3, 1.0))))