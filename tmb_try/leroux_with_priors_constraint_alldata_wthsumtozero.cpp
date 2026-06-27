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

  DATA_SCALAR(beta_prior_sd);
  DATA_SCALAR(tau_prior_shape);
  DATA_SCALAR(tau_prior_scale);
  //DATA_SCALAR(logit_rho_prior_mean);
  //DATA_SCALAR(logit_rho_prior_sd);

  // ── Parameters ────────────────────────────────────────────────────────────
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(phi_free);  // length N-1 -- unconstrained free components
  PARAMETER(log_tau);
  //PARAMETER(logit_rho);

  // ── Derived ───────────────────────────────────────────────────────────────
  Type tau = exp(log_tau);
  Type rho = Type(0.99);
  //Type rho = invlogit(logit_rho);

  int N = y.size();

  // ── Reconstruct phi with exact sum-to-zero constraint ─────────────────────
  //
  //  phi has N components but only N-1 degrees of freedom.  We parameterise
  //  the first N-1 elements freely (phi_free) and set the last element to
  //  minus their sum, so sum(phi) == 0 is satisfied exactly by construction.
  //
  //  This is the standard reparameterisation used in, e.g., mgcv and brms.
  //  It is the only approach that guarantees the constraint at the Laplace mode
  //  when phi is a random effect, because the constraint is baked into the
  //  parameter space itself — not imposed as a penalty on an unconstrained space.
  //
  vector<Type> phi(N);
  Type phi_last = Type(0);
  for (int i = 0; i < N - 1; i++) {
    phi(i)   = phi_free(i);
    phi_last -= phi_free(i);
  }
  phi(N - 1) = phi_last;   // phi[N-1] = -sum(phi[0..N-2])  =>  sum(phi) = 0

  // ── Row sums of W ─────────────────────────────────────────────────────────
  typedef Eigen::SparseMatrix<Type> SpMat;
  typedef typename SpMat::InnerIterator SpIt;

  vector<Type> d(N);
  d.setZero();
  for (int j = 0; j < W.outerSize(); ++j)
    for (SpIt it(W, j); it; ++it)
      d(it.row()) += it.value();

  // ── Quadratic form  phi' Q phi  where  Q = tau*(rho*(D-W) + (1-rho)*I) ───
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
  //
  //  With rho = 0.99 < 1 the Leroux precision matrix Q is full rank (positive
  //  definite), so we use all N eigenvalues.  The sum-to-zero constraint
  //  removes one degree of freedom from phi, but Q itself is not rank-deficient
  //  here, so no eigenvalue needs to be dropped from the log-det.
  //
  //  log|Q| = N*log(tau) + sum_i log(rho*lambda_i + (1-rho))
  //  where lambda_i are the eigenvalues of (D - W).
  //
  Type logdetQ = Type(N) * log_tau;
  for (int i = 0; i < N; i++)
    logdetQ += log(rho * eig_DmW(i) + (Type(1) - rho));

  // ── GMRF prior (N-1 free parameters, full-rank Q) ────────────────────────
  //
  //  The normalised GMRF density for the constrained phi is:
  //
  //    -log p(phi) = -0.5*log|Q| + 0.5*phi'Q*phi + 0.5*N*log(2*pi)
  //                  + 0.5*log(N)    <- Jacobian for the linear constraint
  //
  //  The Jacobian term 0.5*log(N) accounts for the change of variables from
  //  (phi_1,...,phi_N) on the constraint hyperplane to the N-1 free coordinates
  //  (phi_free).  It is a constant w.r.t. the parameters, so it does not affect
  //  optimisation, but it is included for a correctly normalised posterior.
  //
  Type nll = Type(0);
  nll -= Type(0.5) * logdetQ;
  nll += quadform;
  nll += Type(0.5) * log(Type(N));   // constraint Jacobian

  // ── Explicit priors ───────────────────────────────────────────────────────
  for (int j = 0; j < beta.size(); j++)
    nll -= dnorm(beta(j), Type(0), beta_prior_sd, true);

  nll -= dgamma(tau, tau_prior_shape, tau_prior_scale, true);

  //nll -= dnorm(logit_rho, logit_rho_prior_mean, logit_rho_prior_sd, true);

  // ── Likelihood (all N areas; NAs pre-filled with 0 and excluded) ──────────
  vector<Type> eta = X * beta + phi;
  for (int k = 0; k < N; k++)
    nll -= dbinom_robust(y(k), n(k), eta(k), true);

  // ── Report ────────────────────────────────────────────────────────────────
  ADREPORT(tau);
  ADREPORT(rho);
  REPORT(eta);          // raw linear predictor -- used for post-hoc calibration
  REPORT(phi);          // full N-vector, reconstructed from phi_free

  vector<Type> p = invlogit(eta);
  REPORT(p);

  // Diagnostic: should be exactly 0 (machine precision) by construction
  Type phi_sum = phi.sum();
  REPORT(phi_sum);

  return nll;
}
