import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


s = np.array(["aa", "b", "aa", "c"])
m = (s == "aa")
print("s.dtype", str(s.dtype)); print("s", s.tolist())
print("m", m.astype(np.int32).tolist())

