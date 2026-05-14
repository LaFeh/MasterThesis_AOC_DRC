library(rstan)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
setwd("D:/DRC/gaussian_process_AOC")
file.edit("~/.Renviron")

Sys.setenv(
  PATH = paste(
    "C:/rtools45/usr/bin",
    "C:/rtools45/x86_64-w64-mingw32.static.posix/bin",
    "D:/programms/R-4.5.3/bin/x64",
    sep = ";"
  )
)
Sys.setenv(RTOOLS45_HOME = "C:/rtools45")
Sys.setenv(R_MAKEVARS_USER = "")
Sys.setenv(R_WIN_NO_SHORT_PATH = "true")

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
fit <- stan(file = 'schools.stan', data = schools_dat)

##########
library(rstan)

set.seed(1)
N <- 50
x <- seq(-3, 3, length.out = N)
f_true <- sin(x)
p <- 1 / (1 + exp(-f_true))
y <- rbinom(N, 1, p)



data_list <- list(N = N, x = x, y = y)

fit <- stan(
  file = "test_gp_binary.stan",
  data = data_list,
  iter = 5000,
  chains = 6
)

print(fit)

traceplot(fit)
########### version closer to real-life###############################


set.seed(1)
N <- 50

rho_variance = rnorm(N,0.8,1)
rho_variance = LaplacesDemon::invlogit(rho_variance)

p = rnorm(N,0.5,rho_variance)
p = LaplacesDemon::invlogit(p)


y <- rbinom(N, 1, p)
