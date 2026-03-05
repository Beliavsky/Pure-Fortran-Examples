import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1, 2, 3, 4], dtype=np.int32)
cs = np.cumsum(x)
cp = np.cumprod(x)
print("x", x.tolist())
print("cs", cs.tolist())
print("cp", cp.tolist())

