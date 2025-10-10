neggradF2 <- function(beta, y, X, ses, tau.sq, weights) {
  resids <- as.numeric(y - X%*%beta)
  B <- tau.sq/(ses + tau.sq)
  VV <- sqrt(B/(2*ses + tau.sq))
  rr <- VV*resids

  num <- dnorm(rr)*(pnorm(rr) - 1/2)
  den <- 1 - (pnorm(rr) - 0.5)^2
  fixed_grad <- num/den

  ans <- as.numeric(crossprod(X, weights*VV*fixed_grad))
  return(ans)
}
