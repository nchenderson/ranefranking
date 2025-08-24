RankingRegCoefEst <- function(y, X, ses, tau.sq, H=1) {

Q=function(beta, H=H) {
  resids <- as.numeric(Y - X%*%beta)
  B <- tau.sq/(ses + tau.sq)
  VV <- sqrt(B/(2*ses + tau.sq))
  if(H==1) {
    term1 <- sqrt(tau.sq/(2*pi))*mean(VV*dnorm(VV*resids))
  } else if(H==3) {
    der3 <- pnormder(VV*resids, ord=3)
    f1 <- sqrt(tau.sq/(2*pi))
    term1 <- 0.5*f1*mean(VV*dnorm(VV*resids)) - (1/6)*tau.sq*f1*mean((VV^3)*der3)
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
if(ncol(X)==1) {
  if(H==1) {
      beta.rank <- optimize(Q,lower=-10,upper=10, H=1)$minimum
  } else if(H==3) {
      beta.rank <- optimize(Q,lower=-10,upper=10, H=3)$minimum
  } else if(H==5) {
      beta.rank <- optimize(Q,lower=-10,upper=10, H=5)$minimum
  }
} else if(ncol(X) > 1) {
   if(H==1) {
      beta.rank <- optim(rep(0, ncol(X)),fn=Q, H=1)$par
   } else if(H==3) {
      beta.rank <- optim(rep(0, ncol(X)),fn=Q, H=3)$par
   } else if(H==5) {
      beta.rank <- optim(rep(0, ncol(X)),fn=Q, H=5)$par
   }
}
return(beta.rank)
}

