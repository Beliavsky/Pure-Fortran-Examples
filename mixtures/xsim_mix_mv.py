import sys

import numpy as np


def simulate_mixture_mvnorm(n, weights, means, covs, seed=0):
    """Simulate n samples from a finite mixture of multivariate normals.

    covs is expected to be shape (k, d*d), where each row is a flattened
    covariance matrix for one component.
    """
    rng = np.random.default_rng(seed)

    w = np.asarray(weights, dtype=float)
    means_arr = np.asarray(means, dtype=float)
    covs_arr = np.asarray(covs, dtype=float)

    if n <= 0:
        raise ValueError("n must be > 0")
    if w.ndim != 1:
        raise ValueError("weights must be 1D")
    if means_arr.ndim != 2:
        raise ValueError("means must be 2D with shape (k, d)")
    if covs_arr.ndim != 2:
        raise ValueError("covs must be 2D with shape (k, d*d)")

    k = w.size
    if means_arr.shape[0] != k:
        raise ValueError("means first dimension must match number of weights")
    if covs_arr.shape[0] != k:
        raise ValueError("covs first dimension must match number of weights")

    d = means_arr.shape[1]
    if covs_arr.shape[1] != d * d:
        raise ValueError("each covariance row must contain d*d entries")

    w = w / w.sum()
    z = rng.choice(k, size=n, p=w)

    x = np.empty((n, d), dtype=float)
    for j in range(k):
        idx = np.where(z == j)[0]
        if idx.size == 0:
            continue
        cov_j = covs_arr[j].reshape(d, d)
        x[idx] = np.random.multivariate_normal(means_arr[j], cov_j, size=idx.size)

    return x, z


def main(out_path):
    n = 10000
    weights = np.array([0.2, 0.5, 0.3])
    means = np.array([
        [-2.0, 0.0],
        [1.0, 2.0],
        [3.0, -1.5],
    ])
    covs = np.array([
        [0.5, 0.1, 0.1, 0.7],
        [1.0, -0.3, -0.3, 0.8],
        [0.3, 0.0, 0.0, 0.4],
    ])

    x, z = simulate_mixture_mvnorm(n, weights, means, covs, seed=123)

    print("n", n)
    print("weights", weights)
    print("means")
    print(means)
    print("covariances")
    for j in range(covs.shape[0]):
        print(f"component {j}")
        print(covs[j].reshape(means.shape[1], means.shape[1]))

    # Write only observable data (features) as plain text.
    np.savetxt(out_path, x, fmt="%.10f")
    print("wrote data file:", out_path)


if __name__ == "__main__":
    output_path = sys.argv[1] if len(sys.argv) > 1 else "xsim_mix_mv_data.txt"
    main(output_path)
