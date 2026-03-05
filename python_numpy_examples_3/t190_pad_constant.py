import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.array([[1, 2], [3, 4]])
p = np.pad(a, pad_width=1, mode="constant", constant_values=-1)
print("a", a.tolist())
print("p", p.tolist())

