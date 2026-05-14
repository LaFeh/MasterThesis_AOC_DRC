data {
  int<lower=1> N;                 // number of data points
  vector[N] x;                    // inputs (1D)
  int<lower=0,upper=1> y[N];      // binary outputs
}

parameters {
  real<lower=0> alpha;            // signal std
  real<lower=0> rho;              // length-scale
  vector[N] f;                    // latent function values
}

transformed parameters {
  matrix[N, N] K;

  for (i in 1:N) {
    for (j in i:N) {
      real sq_dist = square(x[i] - x[j]);
      K[i, j] = square(alpha) * exp(-0.5 * sq_dist / square(rho));
      K[j, i] = K[i, j];
    }
  }

  // add small jitter for numerical stability
  for (i in 1:N)
    K[i, i] = K[i, i] + 1e-6;
}

model {
  // priors
  alpha ~ normal(0, 1);
  rho ~ normal(0, 1);

  // GP prior on latent function
  f ~ multi_normal(rep_vector(0, N), K);

  // likelihood (logistic link)
  y ~ bernoulli_logit(f);
}

generated quantities {
  vector[N] y_rep;

  for (n in 1:N)
    y_rep[n] = bernoulli_logit_rng(f[n]);
}
