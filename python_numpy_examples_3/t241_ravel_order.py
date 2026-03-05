import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


a = np.arange(1, 13).reshape(3, 4, order="C")
rc = a.ravel(order="C")
rf = a.ravel(order="F")
print("a", a.tolist())
print("rc", rc.tolist())
print("rf", rf.tolist())

