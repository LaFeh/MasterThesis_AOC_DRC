#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator()() {

  // ── Data ──────────────────────────────────────────────────────────────────
  DATA_VECTOR(y);            // response, NAs filled with 0
  DATA_VECTOR(n);            // trials (1 for Bernoulli)
  DATA_MATRIX(X);            // design matrix, all N rows
  DATA_SPARSE_MATRIX(W);     // weighted adjacency [N x N]
  DATA_IVECTOR(obs_idx);     // 0-based indices of observed (non-NA) rows

  // ── Parameters ────────────────────────────────────────────────────────────
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(phi);     // length N -- all areas, observed + missing
  PARAMETER(log_tau);
  PARAMETER(logit_rho);

  // ── Derived ───────────────────────────────────────────────────────────────
  Type tau = exp(log_tau);
  Type eps = Type(0.001);
  Type rho = eps + (Type(1.0) - 2.0*eps) * invlogit(logit_rho);
  //Type rho = invlogit(logit_rho);
  int  N   = y.size();

  typedef Eigen::SparseMatrix<Type> SpMat;
  typedef typename SpMat::InnerIterator SpIt;

  // ── Row sums of W (diagonal of D) ─────────────────────────────────────────
  vector<Type> d(N);
  d.setZero();
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      d(it.row()) += it.value();

  // ── Build sparse Q = tau * [rho*(D-W) + (1-rho)*I] ───────────────────────
  SpMat Q(N, N);
  Q.reserve(W.nonZeros() + N);

  // Off-diagonal: -tau * rho * w_ij
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      Q.insert(it.row(), it.col()) = -tau * rho * it.value();

  // Diagonal: tau * (rho * d_i + (1 - rho))
  for (int i = 0; i < N; i++)
    Q.coeffRef(i, i) += tau * (rho * d(i) + (Type(1) - rho));

  // ── GMRF prior via sparse Cholesky -- same approach as INLA ───────────────
  // GMRF_t builds the sparse Cholesky of Q once and reuses it for:
  //   - log|Q| (from diagonal of Cholesky factor)
  //   - phi' Q phi (via sparse triangular solve)
  // This replaces both the eigenvalue log-det loop and the quadform loop
  using namespace density;
  GMRF_t<Type> gmrf(Q);
  Type nll = gmrf(phi);    // = -0.5*log|Q| + 0.5*phi'Qphi + const

  // ── Sum-to-zero soft constraint on phi ────────────────────────────────────
  // Fixes identifiability between intercept (beta) and mean(phi)
  Type phi_mean = phi.sum() / Type(N);
  vector<Type> phi_c = phi - phi_mean;
  nll += Type(0.5) * Type(N) * tau * phi_mean * phi_mean;

  // ── PC prior on tau ───────────────────────────────────────────────────────
  // P(sigma > 1) = 0.05 where sigma = 1/sqrt(tau), lambda = 3
  Type lambda_pc = Type(3);
  nll -= log(lambda_pc / Type(2)) - lambda_pc / sqrt(tau) + log_tau / Type(2);

  // ── Binomial likelihood (observed areas only) ─────────────────────────────
  vector<Type> eta = X * beta + phi_c;
  for (int k = 0; k < obs_idx.size(); k++) {
    int i = obs_idx(k);
    nll -= dbinom_robust(y(i), n(i), eta(i), true);
  }

  // ── Report ────────────────────────────────────────────────────────────────
  ADREPORT(tau);
  ADREPORT(rho);
  REPORT(eta);       // point estimates, no SE (fast)
  ADREPORT(phi_c);   // SE needed for marginal probability correction in R

  return nll;
}
