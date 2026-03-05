import numpy as np

np.set_printoptions(precision=6, suppress=True, linewidth=120)

a = np.arange(12).reshape(3,4)
b = a.T
c = np.ascontiguousarray(b)
d = np.asfortranarray(a)
print("a.flags.c_contiguous", a.flags.c_contiguous)
print("a.flags.f_contiguous", a.flags.f_contiguous)
print("b.flags.c_contiguous", b.flags.c_contiguous)
print("b.flags.f_contiguous", b.flags.f_contiguous)
print("c.flags.c_contiguous", c.flags.c_contiguous)
print("d.flags.f_contiguous", d.flags.f_contiguous)
