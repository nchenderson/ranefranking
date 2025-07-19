PostRank <- function(beta.hat, Y, X, ses, tau.sq) {
  resids <- as.numeric(Y - X%*%beta.hat)
  B <- tau.sq/(ses + tau.sq)
  VV <- sqrt(B/(2*ses + tau.sq))
  post.rank <- pnorm(VV*resids)
  return(post.rank)
}
