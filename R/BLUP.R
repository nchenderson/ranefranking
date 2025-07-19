BLUP <- function(beta.hat, Y, X, ses, tau.sq) {
  resids <- as.numeric(Y - X%*%beta.hat)
  B <- tau.sq/(ses + tau.sq)
  blup <- B*resids
  return(blup)
}
