

# Optimal hyperplane
# x_1 + ... + x_d = 0.5 * d * mu
fn.opt.hyppln <- function(x, d, mu) {
  if (sum(x) - (0.5 * d * mu) >= 0) {
    y <- 1
  } else {
    y <- 0
  }
  return(y)
}




# Two d-dimensional Gaussian distribution
# Homogeneous covariance: sigma = I
# Fixed mean vector of first Gaussian: mu1 = (0, ..., 0)
# mu2 = (mu, ..., mu)
library(mvtnorm)
d <- c(5, 10 * (1:10))
n <- 1000000
mu <- 0.05 * (1:20)
auc <- matrix(data = 0, nrow = length(mu), ncol = length(d))
rownames(auc) <- mu
colnames(auc) <- d
for (i.d in 1:length(d)) {
  for (i.mu in 1:length(mu)) {
    x.0 <- rmvnorm(n = n, mean = rep(0, d[i.d]), sigma = diag(1, d[i.d]))
    x.1 <- rmvnorm(n = n, mean = rep(mu[i.mu], d[i.d]), sigma = diag(1, d[i.d]))
    y.0 <- apply(X = x.0, MARGIN = 1, FUN = fn.opt.hyppln, mu = mu[i.mu], d = d[i.d])
    y.1 <- apply(X = x.1, MARGIN = 1, FUN = fn.opt.hyppln, mu = mu[i.mu], d = d[i.d])
    auc[i.mu, i.d] <- 0.5 * ((sum(y.0 == 0) / n) + (sum(y.1 == 1) / n))
  }
  print(d[i.d])
}
auc

