import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

# simple polynomial evaluation without np.polyval
x = np.linspace(-1.0, 1.0, 5)
# p(x) = 2x^3 - x + 1
p = 2.0*x**3 - x + 1.0
print("x", x)
print("p", p)
