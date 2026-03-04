import numpy as np


def main():
    c1 = np.array([1.0, 3.0])
    c2 = np.array([2.0, 4.0])
    m = np.stack((c1, c2), axis=1)

    mt = np.transpose(m)
    mt2 = m.T

    cat0 = np.concatenate((m, mt), axis=0)
    cat1 = np.concatenate((m, mt), axis=1)

    v1 = np.array([1, 2])
    v2 = np.array([3])
    catv = np.concatenate((v1, v2), axis=0)

    print("m:")
    print(m)
    print("mt:")
    print(mt)
    print("mt2:")
    print(mt2)
    print("cat0:")
    print(cat0)
    print("cat1:")
    print(cat1)
    print("catv:", catv)


if __name__ == '__main__':
    main()
