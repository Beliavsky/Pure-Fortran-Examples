import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


p = np.array([1.0, -3.0, 2.0])  # x^2 - 3x + 2
x = np.array([-1.0, 0.0, 2.0])
y = np.polyval(p, x)
dp = np.polyder(p)            # derivative: 2x - 3
dy = np.polyval(dp, x)
print("p", p.tolist())
print("x", x.tolist())
print("y", np.round(y, 6).tolist())
print("dp", dp.tolist())
print("dy", np.round(dy, 6).tolist())

