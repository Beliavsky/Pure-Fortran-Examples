import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(1, 9)
print("x", x.tolist())
print("last", int(x[-1]))
print("rev", x[::-1].tolist())
print("rev_step2", x[::-2].tolist())

