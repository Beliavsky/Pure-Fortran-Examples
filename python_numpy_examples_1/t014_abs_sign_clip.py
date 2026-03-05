import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

x = np.array([-3.0, -1.2, 0.0, 2.2, 5.5])
print("abs", np.abs(x))
print("sign", np.sign(x))
print("clip", np.clip(x, -1.0, 3.0))
