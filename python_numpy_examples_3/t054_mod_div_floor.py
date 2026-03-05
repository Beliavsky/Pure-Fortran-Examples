import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([0, 1, 2, 3, 4, 5, 6])
m = x % 4
q = x // 4
qq, rr = np.divmod(x, 4)
print("x", x.tolist())
print("m", m.tolist())
print("q", q.tolist())
print("qq", qq.tolist())
print("rr", rr.tolist())

