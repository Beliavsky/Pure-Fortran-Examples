import numpy as np


def main():
    z = np.zeros(4, dtype=np.int_)
    o = np.ones(4)
    a = np.arange(2, 10, 2)
    t = np.linspace(0.0, 1.0, 5)

    u = np.array([3.0, 1.0, 7.0])
    v = np.array([2.0, 5.0, 0.0])
    m = np.stack((u, v), axis=1)

    mn = np.min(m, axis=1, keepdims=True)
    amn = np.argmin(m, axis=1)

    print("zeros:", z)
    print("ones:", o)
    print("arange:", a)
    print("linspace:", t)
    print("min keepdims:\n", mn)
    print("argmin axis=1:", amn)


if __name__ == '__main__':
    main()

