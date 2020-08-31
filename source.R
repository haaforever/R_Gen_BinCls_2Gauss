

library(mvtnorm)
fn.gen.bincls.2gauss <- function(n.neg, n.pos, mu, d) {
  x.0 <- rmvnorm(n = n.neg, mean = rep(0, d), sigma = diag(1, d))
  x.1 <- rmvnorm(n = n.pos, mean = rep(mu, d), sigma = diag(1, d))
  y.0 <- rep(0, n.neg)
  y.1 <- rep(1, n.pos)
  dt <- data.frame(rbind(x.0, x.1), y = c(y.0, y.1))
  return(dt)
}
