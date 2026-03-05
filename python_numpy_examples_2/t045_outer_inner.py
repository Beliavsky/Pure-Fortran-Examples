import numpy as np

np.set_printoptions(linewidth=200, suppress=True)

x = np.array([1, 2, 3], dtype=np.int64)
y = np.array([4, 5], dtype=np.int64)
o = np.outer(x, y)
inn = np.inner(x, np.array([10, 20, 30], dtype=np.int64))
assert (o == np.array([[4, 5], [8, 10], [12, 15]], dtype=np.int64)).all()
assert inn == 140
print("PASS")
