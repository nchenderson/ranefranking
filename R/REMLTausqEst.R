REMLTausqEst <- function(y, X, ses, trim.quant=0.025) {
     VY <- var(y)
     tausq_hat <- optimize(Q_REML, interval=c(0,4*VY), y=y, X=X, ses=ses)$minimum
     return(tausq_hat)
}
