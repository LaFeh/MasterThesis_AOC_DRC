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

  // ── Precision-weighted centering of phi (INLA parametrisation) ────────────
  // INLA imposes sum_i w_i * phi_i = 0  where  w_i = rho*d_i + (1-rho),
  // the diagonal of Q (up to tau). This makes phi orthogonal to the intercept
  // so beta_0 carries the marginal mean and predictions are centred like INLA.
  // We enforce this exactly by replacing phi with phi* = phi - w-weighted mean,
  // which is a hard zero-cost reparametrisation -- no penalty, no extra parameters.
  Type w_sum     = Type(0);
  Type wphi_sum  = Type(0);
  for (int i = 0; i < N; i++) {
    Type w_i   = rho * d(i) + (Type(1) - rho);
    w_sum     += w_i;
    wphi_sum  += w_i * phi(i);
  }
  Type phi_wmean = wphi_sum / w_sum;   // precision-weighted mean of phi

  vector<Type> phi_c(N);              // centred phi -- used everywhere below
  for (int i = 0; i < N; i++)
    phi_c(i) = phi(i) - phi_wmean;

  // ── Quadratic form (uses centred phi) ────────────────────────────────────
  Type phiDphi = Type(0);
  for (int i = 0; i < N; i++) phiDphi += d(i) * phi_c(i) * phi_c(i);

  Type phiWphi = Type(0);
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      phiWphi += it.value() * phi_c(it.row()) * phi_c(it.col());

  Type phiDWphi = phiDphi - phiWphi;
  Type phiphiT  = (phi_c * phi_c).sum();

  Type quadform = Type(0.5) * tau * (rho * phiDWphi + (Type(1) - rho) * phiphiT);

  // ── Log-determinant of Q ──────────────────────────────────────────────────
  // One eigenvalue of Q is effectively 0 after centering (the constrained
  // direction). Drop it from the log-det sum, matching INLA's rank-(N-1) prior.
  Type logdetQ = Type(N - 1) * log_tau;
  // Sort eigenvalues ascending; eig_DmW(0) is the near-zero one (skip it).
  for (int i = 1; i < N; i++)
    logdetQ += log(rho * eig_DmW(i) + (Type(1) - rho));

  // ── GMRF prior (all N areas) ──────────────────────────────────────────────
  Type nll = Type(0);
  nll -= Type(0.5) * logdetQ;
  nll += quadform;

  // ── Likelihood (observed areas only) ─────────────────────────────────────
  vector<Type> eta = X * beta + phi_c;
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
