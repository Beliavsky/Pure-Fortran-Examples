import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(10, dtype=np.int32)
# create a 2D view of 6 windows of length 5
w = np.lib.stride_tricks.as_strided(x, shape=(6, 5), strides=(x.strides[0], x.strides[0]))
print("x", x.tolist())
print("w.shape", w.shape)
print("w0", w[0].tolist())
print("w5", w[5].tolist())

