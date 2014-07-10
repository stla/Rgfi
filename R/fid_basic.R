
#' Fiducial sample for the basic Gaussian model
#' 
#' @param ylow vector of lower bounds of observations
#' @param yupp vector of upper bounds of observations
#' @param N number of samples to generate
#' @param R threshold fraction for ESS
#' 
#' @examples
#' y <- c(1.624, 2.209, 2.097, 0.558, -0.335, -0.971, -1.65, -2.338, -3.29, -4.291, 2.862, 2.023, -2.336, -0.613, -0.907, 0.354)
#' sims <- fid_basic(y, y+0.001, 1000)
#' VERTEX <- sims$VERTEX; WT <- sims$WT
#' plot(density(VERTEX$mu, weights=WT))
#' curve(dnorm(x,mean(y),sd(y)/sqrt(length(y))), add=TRUE)
#' plot(density(VERTEX$sigma^2, weights=WT))
#' curve(1/x^2*dgamma(1/x, length(y)/2, crossprod(y-mean(y))/2), add=TRUE, col="red")  
#' 
#' @export


fid_basic <- function(ylow, yupp, N, R=0.5){
  dat <- cbind(ylow,yupp)
  n <- nrow(dat)
  X <- FE <-  t(t(rep(1,n)))
  V <- diag(n)
  Y  <-  dat 
  L <- Y[,1] 
  U <- Y[,2] 
  Dim <- 2
  # initial sample
  Z <- matrix(rnorm(n*N), ncol=N) 
  weight  <- matrix(0, nrow=n, ncol=N)
  A <- V %*% Z # A=A[[j]] is the matrix (V_1 Z_1 ... V_re Z_re) with V_re=1 
  C1 <- K_start <- c(1,2)
  Z[-C1,] <- 0
  h <- function(M){ 
    R1 <- c(alow=L[K_start[1]], aupp=U[K_start[1]], b=-M[K_start[1],1])
    R2 <- c(alow=L[K_start[2]], aupp=U[K_start[2]], b=-M[K_start[2],1])
    return(ipart(R1,R2))
  }
  VTall <- lapply(1:N, function(j) h(as.matrix(A[,j]))) 

  #### sampling k=3, ..., n -----------------------
  ESS <- rep(NA,n)
  for(k in 3:n){
    Z1 <- FE[k,]
    VTall_new <- vector(mode = "list", length=N)
    U <- runif(N)
    for(j in 1:N){
      VTj <- VTjcopy <- VTall[[j]]
      VTjcopy[2,] <- Z1*VTj[2,]
      mM <- findSupport(VTjcopy, L[k], U[k]) 
      y <- atan(mM[2]) 
      x <- atan(mM[1]) 
      u <- x+(y-x)*U[j]  #runif(1, x, y) 
      ZZ <- Z[k,j] <- tan(u)  
      wt <- weight[k,j] <- (-ZZ^2/2)+log(1+ZZ^2)+log(y-x) 
      # new polygon 
      D31 <- c(
        a=L[k],
        b=ZZ,
        typ=FALSE
      )
      D32 <- c(
        a=U[k],
        b=ZZ,
        typ=1
      )
      Z[k,j] <- -ZZ
      VTj_update <- updatePoly(VTj, D31)
      VTj_update <- VTall_new[[j]] <- updatePoly(VTj_update, D32)
    } # END for(j in 1:N)
    VTall <- VTall_new
    
    ### calculate weights   ---------------------------
    WT <- apply(weight,2,cumsum)  #only last re is restricted
    WT <- WT[nrow(WT),]
    if(sum(WT)==-Inf){
      stop("Error: possible underflow")
    }
    WT <- exp(WT)/sum(exp(WT))
    ESS[k] <- 1/crossprod(WT)
    
    ### alteration --------------------------------------
    if(ESS[k] < R*N && k<n){
      Nsons <- rmultinom(1, N, WT)[,1]
      Zt <- Z[1:k,]
      Znew <- array(NA, dim=c(k,N))
      VTnew <- vector(mode = "list", length=N)
      start <- 0
      for(j in which(Nsons!=0)){
        ncopies <- Nsons[j]
        VTj <- VTall[[j]]
        VTnew[[start+1]] <- VTj
        Znew[,start+1] <- Zt[,j]
        if(ncopies>1){
          alt <- alterate(ncopies-1, VTj, Zt[,j], k-1) 
          Znew[,(start+2):(start+ncopies)] <- alt$Znew
          VTnew[(start+2):(start+ncopies)] <- alt$VTnew
        }
        start <- start+ncopies
      }
      VTall <- VTnew
      Z[1:k,] <- Znew
      
      #
      weight  <- array(0, dim=c(n,N))
    } # END ALTERATION 
    
  } # END for(k in 3:n)
  
  #----------------------------------------------------determine signs
  re <- 1
  signs=matrix(0, nrow=re, ncol=N)    #  
  for(i in 1:N){
    for(j in 1:re){
      if(all(VTall[[i]][1,]>0)){ #i.e. all are positive
        signs[j,i] <- 1
      }
      else if(all(VTall[[i]][1,]<0)){ #i.e. all are negative
        signs[j,i] <- -1 
      }
    }
  }
  
  #----------------------------------------------------FINAL RESAMPLE  		
  for(j in 1:N){
    alt <- alterate(1, VTall[[j]], Z[,j], n-1)
    VTall[j] <- alt$VTnew
  }
  
  
  #----------------------------------------------------flip negatives  	
  for(i in 1:N){
    for(j in 1:re){ #only look at random effects
      if(signs[j,i]==-1){
        VTall[[i]][1,] <- -VTall[[i]][1,]  #only need to flip the negatives         
      }
    }
  } 
  
  #----------------------------------------------------pick coordinates
  Dim <- 2
  VT_end <- matrix(0, nrow=Dim, ncol=N) 
  for(i in 1:N){
    #for(j in 1:Dim){
    if(runif(1)<=0.5){
      #if(j<=fe){
      VT_end[2,i] <- min(VTall[[i]][2,])
      #}else{
      VT_end[1,i] <- max(min(VTall[[i]][1,]),0) 
      #}
    }else{
      #if(j<=fe){
      VT_end[2,i] <- max(VTall[[i]][2,])
      #}else{
      VT_end[1,i] <- max(max(VTall[[i]][1,]),0) 
      #}
    }
  }
  
  return(list(VERTEX=list(mu=VT_end[2,], sigma=VT_end[1,]), WEIGHT = WT))
}








