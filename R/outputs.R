#' Analysis of fiducial simulations
#' 
#' @export
inference <- function(vertex, weight, alpha=0.05){ 
  out <- rep(NA,4)
  names(out) <- c("mean","median","low","up")
  out[1] <- sum(vertex*weight)
  h <- cbind(vertex,weight)
  hsort <- gx.sort(h,1)
  hsum <- cumsum(hsort[,2])
  ci_u <- min(which(hsum>=1-alpha/2)) #upper confidence bound
  ci_l <- min(which(hsum>=alpha/2))   #lower confidence bound
  ci_m <- min(which(hsum>=0.5))
  out[3] <- hsort[ci_l,1]  #lower bound
  out[4] <- hsort[ci_u,1] #upper bound
  out[2] <- hsort[ci_m,1] #estimate
  out
}