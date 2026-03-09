import numpy as np
import sys


def fit_gmm_em(x, k=3, max_iter=200, tol=1e-6, reg=1e-6, seed=123):
    n, d = x.shape
    if n < k:
        raise ValueError("number of samples must be >= number of components")

    rng = np.random.default_rng(seed)

    idx = rng.choice(n, size=k, replace=False)
    means = x[idx].copy()
    weights = np.full(k, 1.0 / k)

    global_cov = np.cov(x, rowvar=False)
    if np.ndim(global_cov) == 0:
        global_cov = np.array([[float(global_cov)]])
    base_cov = global_cov + reg * np.eye(d)
    covs = np.empty((k, d * d), dtype=float)
    for j in range(k):
        covs[j] = base_cov.reshape(d * d)

    prev_ll = -np.inf
    for it in range(1, max_iter + 1):
        # E-step
        log_prob = np.empty((n, k), dtype=float)
        for j in range(k):
            cov_j = covs[j].reshape(d, d)
            diff = x - means[j]
            cov_local = cov_j + 1e-12 * np.eye(d)
            sign, logdet = np.linalg.slogdet(cov_local)
            if sign <= 0:
                cov_local = cov_local + 1e-6 * np.eye(d)
                sign, logdet = np.linalg.slogdet(cov_local)
            inv_cov = np.linalg.inv(cov_local)
            log_prob[:, j] = np.log(weights[j] + 1e-300) - 0.5 * (
                d * np.log(2.0 * np.pi) + logdet + np.einsum("ni,ij,nj->n", diff, inv_cov, diff)
            )

        amax = np.max(log_prob, axis=1)
        s = np.sum(np.exp(log_prob - amax[:, None]), axis=1)
        log_norm = amax + np.log(s)
        ll = float(np.sum(log_norm))
        resp = np.exp(log_prob - log_norm[:, None])

        # M-step
        nk = np.sum(resp, axis=0) + 1e-15
        weights = nk / n
        means = resp.T @ x
        for j in range(k):
            means[j] = means[j] / nk[j]

        for j in range(k):
            diff = x - means[j]
            wdiff = diff * resp[:, j][:, None]
            cov_j = (wdiff.T @ diff) / nk[j]
            cov_j = cov_j + reg * np.eye(d)
            covs[j] = cov_j.reshape(d * d)

        if abs(ll - prev_ll) < tol * (1.0 + abs(ll)):
            return weights, means, covs, ll, it
        prev_ll = ll

    return weights, means, covs, prev_ll, max_iter


def main():
    data_file = "xsim_mix_mv_data.txt"
    k = 3
    max_iter = 200
    tol = 1e-6
    reg = 1e-6
    seed = 123
    if len(sys.argv) > 1:
        data_file = sys.argv[1]

    x = np.loadtxt(data_file, dtype=float)
    x = np.atleast_2d(x)
    if x.shape[0] == 1 and x.shape[1] > 1:
        # Keep one sample with d features.
        pass

    w, mu, sigma, ll, niter = fit_gmm_em(
        x,
        k=k,
        max_iter=max_iter,
        tol=tol,
        reg=reg,
        seed=seed,
    )

    order = np.argsort(mu[:, 0])
    w = w[order]
    mu = mu[order]
    sigma = sigma[order]

    print("data file:", data_file)
    print("shape", x.shape)
    print("components", k)
    print("iterations", niter)
    print("log_likelihood", ll)
    print("weights", w)
    print("means")
    print(mu)
    print("covariances")
    for j in range(k):
        print(f"component {j}")
        print(sigma[j].reshape(x.shape[1], x.shape[1]))


if __name__ == "__main__":
    main()
