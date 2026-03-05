import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 10).reshape(3, 3)
t = np.tri(3, 3, k=0, dtype=np.int32)
lo = np.tril(a, k=0)
up = np.triu(a, k=1)
print("a", a.tolist())
print("t", t.tolist())
print("lo", lo.tolist())
print("up", up.tolist())

