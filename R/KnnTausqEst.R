KnnTausqEst <- function(y, X, ses, trim.quant=0.025, k = 1) {
  ### Note that this function will not work for the case
  ### when X only has a single intercept column

  ### This estimator is based on the idea of
  ## "A nearest neighbor estimate of the
  ## residual variance". Devroye Electronic J of Stat (2018)
  ## That paper mostly focuses on k=1. Our simulations seem
  ## to confirm that this is typically the best choice of k

  n <- nrow(X)
  muhat <- dists <- rep(NA, n)
  dists <- rep(NA, n)
  ## May want to try a more conservative winsorization e.g. probs = c(0.05, 0.95)
  ## (at least as a backup)
  if(trim.quant > 0) {
     qq <- quantile(y, probs=c(trim.quant, 1 - trim.quant))
     y.winsor <- y
     y.winsor[y < qq[1]] <- qq[1]
     y.winsor[y > qq[2]] <- qq[2]
  } else{
     y.winsor <- y
  }
  for (i in 1:n) {
    for(j in 1:n) {
      # Compute distances to all training points
      dists[j] <- sum((X[i,] - X[j,])*(X[i,] - X[j,]))
      # Find indices of k nearest neighbors
    }
    nn_idx <- order(dists)[1:(k+1)]
    nn_idx <- nn_idx[-1]
    # Average the y values of the neighbors (not including i)
    muhat[i] <- mean(y.winsor[nn_idx])
  }
  ## With n = 50, probs=c(0.025, 0.975) probably makes sense

  Sstar <- mean(y.winsor*muhat)
  Lstar <- mean(y.winsor*y.winsor) - Sstar
  tausq_hat <- max(Lstar - mean(ses), 0)
  if(tausq_hat > 1000) {
    print(c(Sstar, mean(y*y), mean(ses), var(muhat)))
    print(summary(y))
    hist(y)
  }
  return(tausq_hat)
}
