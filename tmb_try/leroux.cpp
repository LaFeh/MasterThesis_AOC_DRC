#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator()() {

  // ── Data ──────────────────────────────────────────────────────────────────
  DATA_VECTOR(y);            // response, NAs filled with 0
  DATA_VECTOR(n);            // trials (1 for Bernoulli)
  DATA_MATRIX(X);            // design matrix, all N rows
  DATA_SPARSE_MATRIX(W);     // weighted adjacency [N x N]
  DATA_VECTOR(eig_DmW);      // eigenvalues of (D - W), length N
  DATA_IVECTOR(obs_idx);     // 0-based indices of observed (non-NA) rows

  // ── Parameters ────────────────────────────────────────────────────────────
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(phi);     // length N -- all areas, observed + missing
  PARAMETER(log_tau);
  PARAMETER(logit_rho);

  // ── Derived ───────────────────────────────────────────────────────────────
  Type tau = exp(log_tau);
  Type rho = invlogit(logit_rho);
  int  N   = y.size();

  // ── Row sums of W ─────────────────────────────────────────────────────────
  typedef Eigen::SparseMatrix<Type> SpMat;
  typedef typename SpMat::InnerIterator SpIt;

  vector<Type> d(N);
  d.setZero();
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      d(it.row()) += it.value();

  // ── Quadratic form ────────────────────────────────────────────────────────
  Type phiDphi = Type(0);
  for (int i = 0; i < N; i++) phiDphi += d(i) * phi(i) * phi(i);

  Type phiWphi = Type(0);
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      phiWphi += it.value() * phi(it.row()) * phi(it.col());

  Type phiDWphi = phiDphi - phiWphi;
  Type phiphiT  = (phi * phi).sum();

  Type quadform = Type(0.5) * tau * (rho * phiDWphi + (Type(1) - rho) * phiphiT);

  // ── Log-determinant of Q ──────────────────────────────────────────────────
  Type logdetQ = Type(N) * log_tau;
  for (int i = 0; i < N; i++)
    logdetQ += log(rho * eig_DmW(i) + (Type(1) - rho));

  // ── GMRF prior (all N areas) ──────────────────────────────────────────────
  Type nll = Type(0);
  nll -= Type(0.5) * logdetQ;
  nll += quadform;

  // ── Likelihood (observed areas only) ─────────────────────────────────────
  vector<Type> eta = X * beta + phi;
  for (int k = 0; k < obs_idx.size(); k++) {
    int i = obs_idx(k);
    nll -= dbinom_robust(y(i), n(i), eta(i), true);
  }

  // ── Report ────────────────────────────────────────────────────────────────
  ADREPORT(tau);
  ADREPORT(rho);
  vector<Type> p = invlogit(eta);   // predicted probability all N areas
  ADREPORT(p);

  return nll;
}
