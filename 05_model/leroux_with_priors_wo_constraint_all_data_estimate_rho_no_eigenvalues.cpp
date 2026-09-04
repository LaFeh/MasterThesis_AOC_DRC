#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator()() {
  
  // ── Data ──────────────────────────────────────────────────────────────────
  DATA_VECTOR(y);            // response, NAs filled with 0
  DATA_VECTOR(n);            // trials (1 for Bernoulli)
  DATA_MATRIX(X);            // design matrix, all N rows
  DATA_SPARSE_MATRIX(W);     // weighted adjacency [N x N]
  DATA_IVECTOR(obs_idx);     // 0-based indices of observed (non-NA) rows
  // NOTE: eig_DmW is gone -- no more dense eigendecomposition of (D - W)
  
  DATA_SCALAR(beta_prior_sd);
  DATA_SCALAR(tau_prior_shape);
  DATA_SCALAR(tau_prior_scale);
  DATA_SCALAR(logit_rho_prior_mean);
  DATA_SCALAR(logit_rho_prior_sd);
  
  // ── Parameters ────────────────────────────────────────────────────────────
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(phi);     // length N -- all areas, observed + missing
  PARAMETER(log_tau);
  PARAMETER(logit_rho);
  
  // ── Derived ───────────────────────────────────────────────────────────────
  Type tau = exp(log_tau);
  Type rho = invlogit(logit_rho);
  
  int N = y.size();
  
  typedef Eigen::SparseMatrix<Type> SpMat;
  typedef typename SpMat::InnerIterator SpIt;
  typedef Eigen::Triplet<Type> Trip;
  
  // ── Row sums of W (diagonal of D) ───────────────────────────────────────
  vector<Type> d(N);
  d.setZero();
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      d(it.row()) += it.value();
  
  // ── Build the sparse precision matrix directly ──────────────────────────
  // Q = tau * ( rho * (D - W) + (1 - rho) * I )
  //   off-diagonal: Q_ij = -tau * rho * W_ij
  //   diagonal:     Q_ii =  tau * (rho * d_i + (1 - rho))
  // Same sparsity pattern as W (plus the diagonal) -- never a dense N x N object.
  std::vector<Trip> trip;
  trip.reserve(W.nonZeros() + N);
  
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      trip.push_back(Trip(it.row(), it.col(), -tau * rho * it.value()));
  
  for (int i = 0; i < N; i++)
    trip.push_back(Trip(i, i, tau * (rho * d(i) + (Type(1) - rho))));
  
  SpMat Q(N, N);
  Q.setFromTriplets(trip.begin(), trip.end());
  
  // ── GMRF prior (all N areas) ─────────────────────────────────────────────
  // density::GMRF(Q)(phi) = 0.5*phi'Qphi - 0.5*log|Q| + 0.5*N*log(2*pi),
  // evaluated via a SPARSE CHOLESKY factorization of Q -- this replaces the
  // quadform + eigenvalue-sum block entirely, with no need for eig_DmW.
  // (It differs from your old nll by the constant 0.5*N*log(2*pi), which
  // doesn't depend on any parameter and so doesn't affect the optimum,
  // gradients, or SEs -- only the raw nll value you'd see printed.)
  Type nll_gmrf = density::GMRF(Q)(phi);
  REPORT(nll_gmrf);
  
  Type nll = nll_gmrf;
  
  for (int j = 0; j < beta.size(); j++)
    nll -= dnorm(beta(j), Type(0), beta_prior_sd, true);
  nll -= dgamma(tau, tau_prior_shape, tau_prior_scale, true) + log_tau;
  nll -= dnorm(logit_rho, logit_rho_prior_mean, logit_rho_prior_sd, true);
  
  Type nll_after_priors = nll;
  REPORT(nll_after_priors);
  
  vector<Type> eta = X * beta + phi;
  for (int k = 0; k < N; k++)
    nll -= dbinom_robust(y(k), n(k), eta(k), true);
  
  Type nll_after_lik = nll;
  REPORT(nll_after_lik);
  
  // ── Report ────────────────────────────────────────────────────────────────
  ADREPORT(tau);
  ADREPORT(rho);
  REPORT(eta);
  REPORT(phi);
  
  vector<Type> p = invlogit(eta);
  REPORT(p);
  
  return nll;
}