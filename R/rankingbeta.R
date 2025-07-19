rankingbeta <- function(Y, X, tau.sq, ses, H=1) {
   if(ncol(X) > 1) {
       beta.rank <- optim(par=rep(0, ncol(X)),fn=Qfn, tau.sq=tau.sq, ses=ses, H=H)$par
   } else if(ncol(X)==1) {
       beta.rank <- optimize(Q,lower=-10,upper=10, tau.sq=tau.sq, ses=ses, H=H)$minimum
   }
   return(beta.rank)
}
