Qfn <- function(beta, tau.sq, ses, H=1) {
  ## ses - standard errors squared.
  resids <- as.numeric(Y - X%*%beta)
  B <- tau.sq/(ses + tau.sq)
  VV <- sqrt(B/(2*ses + tau.sq))

  if(H==1) {
    term1 <- sqrt(tau.sq/(2*pi))*mean(VV*dnorm(VV*resids))
  } else if(H==3) {
    der3 <- pnormder(VV*resids, ord=3)
    f1 <- sqrt(tau.sq/(2*pi))
    term1 <- 0.5*f1*mean(VV*dnorm(VV*resids)) - (1/6)*tausq*f1*mean((VV^3)*der3)
  } else if(H==5) {
    der1 <- pnormder(VV*resids, ord=1)
    der3 <- pnormder(VV*resids, ord=3)
    der5 <- pnormder(VV*resids, ord=5)
    f1 <- sqrt(tau.sq/(2*pi))
    term1 <- (5/8)*f1*mean(VV*der1) + (1/12)*f1*tau.sq*mean((VV^3)*der3) + (1/40)*f1*tau.sq*tau.sq*mean((VV^5)*der5)
  }
  midrank_discr <- pnorm(VV*resids) - 0.5
  ans <- -2*term1 + sum(midrank_discr*midrank_discr)
  return(ans)
}
