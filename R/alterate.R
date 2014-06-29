#' Alteration
#' 
#' @export

alterate <- function(ncopies, vt, Ztj, df){ # vt=opoly ; attention ncopies=ncopies-1 dans le resample
  VTnew <- vector(mode="list", length=ncopies) # could use an array instead
  vt <- vt[2:1,]
  C <- mean(Ztj)
  D <- sqrt(crossprod(Ztj-C)[1,1])
  tau <- (Ztj-C)/D 
  Ctil <- rnorm(ncopies, 0, 1/sqrt(df+1))
  Dtil <- sqrt(rchisq(ncopies, df))
  Znew <- array(NA, dim=c(length(Ztj), ncopies))
  for(i in 1:ncopies){ 
    Znew[,i] <- Dtil[i]*tau + Ctil[i] # est-ce utile de calculer Znew ? - oui pour les altérations futures je pense
    vtnew <- array(NA, dim=dim(vt))
    for(m in 1:ncol(vt)){
      munew <- vt[1,m] + vt[2,m]*(C-Ctil[i]*D/Dtil[i])
      sigmanew <- vt[2,m]*D/Dtil[i]
      vtnew[,m] <- c(munew,sigmanew) 
    }
    VTnew[[i]] <- vtnew[2:1,]
  }
  return(list(Znew=Znew, VTnew=VTnew))
}
