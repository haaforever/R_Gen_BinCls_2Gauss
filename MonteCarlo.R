

# Optimal hyperplane
# x_1 + ... + x_d = 0.5 * d * mu
fn.opt.hyppln <- function(x, d, mu) {
  # if (sum(x) - (0.5 * d * mu) >= 0) {
  #   y <- 1
  # } else {
  #   y <- 0
  # }
  # return(y)
  return(sum(x) - (0.5 * d * mu))
}




# Two d-dimensional Gaussian distribution
# Homogeneous covariance: sigma = I
# Fixed mean vector of first Gaussian: mu1 = (0, ..., 0)
# mu2 = (mu, ..., mu)
library(mvtnorm)
library(ROCR)
d <- c(5, 10 * (1:10))
n <- 10000
mu <- 0.05 * (1:20)
auc <- matrix(data = 0, nrow = length(mu), ncol = length(d))
rownames(auc) <- mu
colnames(auc) <- d
n.rep <- 100
for (i in 1:n.rep) {
  for (i.d in 1:length(d)) {
    # i.d <- 1
    for (i.mu in 1:length(mu)) {
      # i.mu <- 1
      x.0 <- rmvnorm(n = n, mean = rep(0, d[i.d]), sigma = diag(1, d[i.d]))
      x.1 <- rmvnorm(n = n, mean = rep(mu[i.mu], d[i.d]), sigma = diag(1, d[i.d]))
      y.0 <- apply(X = x.0, MARGIN = 1, FUN = fn.opt.hyppln, mu = mu[i.mu], d = d[i.d])
      y.1 <- apply(X = x.1, MARGIN = 1, FUN = fn.opt.hyppln, mu = mu[i.mu], d = d[i.d])
      # auc[i.mu, i.d] <- 0.5 * ((sum(y.0 == 0) / n) + (sum(y.1 == 1) / n))
      auc[i.mu, i.d] <- auc[i.mu, i.d] + performance(prediction.obj = prediction(predictions = c(y.0, y.1), labels = c(rep(-1, n), rep(1, n))),
                                    measure = "auc")@"y.values"[[1]]
    }
    print(d[i.d])
  }
  print(paste0(i, "-th iteration"))
}
auc <- auc / n.rep

