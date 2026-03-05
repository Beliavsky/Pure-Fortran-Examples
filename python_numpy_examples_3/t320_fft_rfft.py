import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([0.0, 1.0, 0.0, -1.0])
y = np.fft.rfft(x)
z = np.fft.irfft(y, n=x.size)
print("x", np.round(x, 6).tolist())
print("y_real", np.round(y.real, 6).tolist())
print("y_imag", np.round(y.imag, 6).tolist())
print("z", np.round(z, 6).tolist())

