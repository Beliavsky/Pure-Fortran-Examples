import numpy as np
np.set_printoptions(linewidth=200, suppress=True)
np.random.seed(0)


x = np.array([1.0, np.nan, 3.0, np.nan, 5.0])
m = np.isnan(x)
y = np.nan_to_num(x, nan=-99.0)
nm = np.nanmean(x)
print("x", [None if np.isnan(v) else float(np.round(v,6)) for v in x.tolist()])
print("m", m.astype(np.int32).tolist())
print("y", np.round(y, 6).tolist())
print("nm", float(np.round(nm, 6)))

