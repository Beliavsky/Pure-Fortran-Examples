import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.arange(10)
y = np.arange(1, 13).reshape(3, 4)
print("x", x.tolist())
print("x_3", int(x[3]))
print("x_2_7_2", x[2:7:2].tolist())
print("y", y.tolist())
print("y_1_2", int(y[1,2]))
print("y_row1", y[1,:].tolist())
print("y_col2", y[:,2].tolist())
print("y_block", y[0:2, 1:4].tolist())

