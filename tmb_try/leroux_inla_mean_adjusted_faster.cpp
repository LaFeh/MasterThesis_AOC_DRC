#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator()() {

  // ── Data ──────────────────────────────────────────────────────────────────
  DATA_VECTOR(y);            // response, NAs filled with 0
  DATA_VECTOR(n);            // trials (1 for Bernoulli)
  DATA_MATRIX(X);            // design matrix, all N rows
  DATA_SPARSE_MATRIX(W);     // weighted adjacency [N x N]
  DATA_VECTOR(eig_DmW);      // eigenvalues of (D-W), computed once in R
  DATA_IVECTOR(obs_idx);     // 0-based indices of observed (non-NA) rows

  // ── Parameters ────────────────────────────────────────────────────────────
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(phi);
  PARAMETER(log_tau);
  PARAMETER(logit_rho);

  // ── Derived ───────────────────────────────────────────────────────────────
  Type tau = exp(log_tau);
  Type rho = invlogit(logit_rho);
  int  N   = y.size();

  typedef Eigen::SparseMatrix<Type> SpMat;
  typedef typename SpMat::InnerIterator SpIt;

  // ── Row sums of W ─────────────────────────────────────────────────────────
  vector<Type> d(N);
  d.setZero();
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      d(it.row()) += it.value();

  // ── Log-determinant of Q ──────────────────────────────────────────────────
  // Q = tau * [rho*(D-W) + (1-rho)*I]
  // eigenvalues of Q = tau * [rho*lambda_i(D-W) + (1-rho)]
  // log|Q| = N*log(tau) + sum_i log(rho*lambda_i + (1-rho))
  // eig_DmW computed ONCE in R with RSpectra -- not repeated per iteration
  Type logdetQ = Type(N) * log_tau;
  for (int i = 0; i < N; i++)
    logdetQ += log(rho * eig_DmW(i) + (Type(1) - rho));

  // ── Quadratic form phi' Q phi via sparse matvec -- O(nnz) ─────────────────
  // Build Q phi directly without building Q explicitly
  // Q phi = tau * [rho*(D-W) + (1-rho)*I] * phi
  //       = tau * [rho*D*phi - rho*W*phi + (1-rho)*phi]

  // W*phi via sparse iterator
  vector<Type> Wphi(N);
  Wphi.setZero();
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      Wphi(it.row()) += it.value() * phi(it.col());

  // Q*phi = tau * [rho*(d*phi - Wphi) + (1-rho)*phi]
  vector<Type> Qphi(N);
  for (int i = 0; i < N; i++)
    Qphi(i) = tau * (rho * (d(i) * phi(i) - Wphi(i)) + (Type(1) - rho) * phi(i));

  Type quadform = Type(0.5) * (phi * Qphi).sum();

  // ── GMRF prior over all N areas ───────────────────────────────────────────
  Type nll = Type(0);
  nll -= Type(0.5) * logdetQ;
  nll += quadform;

  // ── Sum-to-zero soft constraint on phi ────────────────────────────────────
  Type phi_mean = phi.sum() / Type(N);
  vector<Type> phi_c = phi - phi_mean;
  nll += Type(0.5) * Type(N) * tau * phi_mean * phi_mean;

  // ── PC prior on tau ───────────────────────────────────────────────────────
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
  REPORT(eta);
  ADREPORT(phi_c);

  return nll;
}
