import numpy as np


def main():
    a = np.array([1.0, 2.0, 3.0])
    z = np.zeros_like(a)
    o = np.ones_like(a)
    f = np.full_like(a, 2.5)

    d = np.dot(a, o)

    c1 = np.array([1.0, 3.0])
    c2 = np.array([2.0, 4.0])
    m = np.stack((c1, c2), axis=1)
    v = np.array([5.0, 6.0])
    mv = np.matmul(m, v)
    mm = np.matmul(m, m)
    at_mv = m @ v
    at_mm = m @ m

    print("z:", z)
    print("o:", o)
    print("f:", f)
    print("dot:", d)
    print("mv:", mv)
    print("mm:")
    print(mm)
    print("@ mv:", at_mv)
    print("@ mm:")
    print(at_mm)


if __name__ == '__main__':
    main()
