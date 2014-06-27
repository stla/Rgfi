library(Rgfi)

## fonction qui retourne m et M 
## arguments : (VTsum, sigma_re), (L[1:k], U[1:k])  
#findSupport <- findSupport # attention il y a un moins par rapport à la fction précédente (je ne sais plus pourquoi)

#library(rgr)
set.seed(666)
#options(error=browser)

dat <- rbind(c(1.624 ,1.625), c(2.209,2.210), c(2.097,2.098), c(0.558,0.559), c(-0.335,-0.334), c(-0.971,-0.970), c(-1.650,-1.649), c(-2.338,-2.337), c(-3.290,-3.289), c(-4.291,-4.290), c(2.862 ,2.863), c(2.023,2.024), c(-2.336,-2.335), c(-0.613,-0.612), c(-0.907,-0.906), c(0.354,0.355))

#y <- seq(1,20, by=2) #sort(round(rnorm(n),3))
#dat <- cbind(y,y+0.05)

n <- nrow(dat)
X <- FE <-  t(t(rep(1,n)))
V <- diag(n)
Y  <-  dat 
L <- Y[,1] 
U <- Y[,2] 

Dim <- 2
N <- nsims <- 4000



# initial sample

Z <- matrix(rnorm(n*N), ncol=N) 
weight  <- matrix(0, nrow=n, ncol=N)
A <- V %*% Z # A=A[[j]] is the matrix (V_1 Z_1 ... V_re Z_re) with V_re=1 
# ici A[[j]] est une seulement colonne et on les empile en colonnes par simulation : A="cbind(A[[1:N]] 
#A <- matrix(A, ncol=1)

C1 <- K_start <- c(1,2)
Z[-C1,] <- 0

# maintenant on calcule les sommets de Q1 dans V 
b <- UL <- c(U,-L) 

h <- function(M){ 
  #M <- as.matrix(M) # quand M colonne - donc uniquement quand re=1 
  #   AA <- cbind(rbind(FE,-FE), rbind(M,-M))
  #   #
  #   a1 <- AA[c(K_start,K_start+n),]
  #   b1 <- b[c(K_start,K_start+n)]
  #   makeH(a1,b1)  
  R1 <- c(alow=L[K_start[1]], aupp=U[K_start[1]], b=M[K_start[1],1])
  R2 <- c(alow=L[K_start[2]], aupp=U[K_start[2]], b=M[K_start[2],1])
return(ipart(R1,R2))
}

j <- 1
h(as.matrix(A[,j]))

VTall <- lapply(1:N, function(j) h(as.matrix(A[,j]))) 
#str(VTall)

ESS <- rep(NA,n)

# sampling k=3, ..., n 
for(k in 3:n){
  Z1 <- FE[k,]
  
  VTall_new <- vector(mode = "list", length=N)
  
  for(j in 1:N){
    VTj <- VTjcopy <- VTall[[j]]
    #sigma_re <- c(VTj[active, c("x1","x2")])
    #VTsum <-  Z1*c(VTj[active,c("y1","y2")])
    
    VTjcopy[2,] <- Z1*VTj[2,]
    mM <- findSupport(VTjcopy, L[k], U[k]) 
    # # test 
    # plotPart(VTj, R=c(alow=L[k], aupp=U[k], b=mM[1]) ) #, xlim=c(-1.7,0),ylim=c(-0.23,3))
    # sample z3
    y <- atan(mM[2]) # atan(mM[2])/pi+.5
    x <- atan(mM[1]) # atan(mM[1])/pi+.5
    u <- runif(1, x, y) #runif(1, x, y)
    ZZ <- Z[k,j] <- tan(u)  # tan(pi*(u-.5)) 
    wt <- weight[k,j] <- (-ZZ^2/2)+log1p(ZZ^2)+log(y-x) # -log(pi) useless constant 
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
    VTj_update <- updatePoly(VTj, D31)
    VTj_update <- VTall_new[[j]] <- updatePoly(VTj_update, D32)
    # # test 
    # plotPart(VTj, R=c(alow=L[k], aupp=U[k], b=ZZ), xlim=c(-1.7,-1.5),ylim=c(-0.23,-0.1))
    # plotPart(VTj_update, R=c(alow=L[k], aupp=U[k], b=ZZ), xlim=c(-1.7,-1.5),ylim=c(-0.23,-0.1))
    # abline(a=L[1], b=-Z[1,j])  
    # abline(a=U[1], b=-Z[1,j])  
    # abline(a=L[2], b=-Z[2,j])  
    # abline(a=U[2], b=-Z[2,j])  
    # abline(a=L[3], b=-Z[3,j], col="red")  
    # abline(a=U[3], b=-Z[3,j], col="red")  
  } # END for(j in 1:N)
  VTall_prev <- VTall
  VTall <- VTall_new
  
  
  WT <- apply(weight,2,cumsum)  #only last re is restricted
  WT <- WT[nrow(WT),]
  if(sum(WT)==-Inf){
    stop("Error: possible underflow")
  }
  WT <- exp(WT)/sum(exp(WT))
  ESS[k] <- 1/crossprod(WT)
  
  print(ESS[k])
  
  ### alteration - uniquement le actifs 
  if(ESS[k] < .5*N && k<n){
    Nsons <- rmultinom(1, N, WT)[,1]  
    Zt <- Z[1:k,]
    Znew <- array(NA, dim=c(k,N))
    VTnew <- vector(mode = "list", length=N)
    start <- 0
    # faire fonction alterate(VTall, Zt)
    # avec sous-fonction alterate(Vtj, ncopies-1)
    for(j in which(Nsons!=0)){
      ncopies <- Nsons[j]
      VTj <- VTall[[j]]
      VTnew[[start+1]] <- VTj
      Znew[,start+1] <- Zt[,j]
      if(ncopies>1){
        alt <- alterate(ncopies-1, VTj, Zt[,j], k-1) 
        Znew[,(start+2):(start+ncopies)] <- alt$Znew
        VTnew[(start+2):(start+ncopies)] <- alt$VTnew
        #print("o")
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

VERTEX <- VT_end 
WEIGHT <- WT 



y <- dat[,1]
print(confint(lm(y~1)))

print(inference(VERTEX[2,],WT))
print(inference(VERTEX[1,],WT))

plot(density(VERTEX[2,], weights=WT))
plot(density(VERTEX[1,], weights=WT))
curve((n-1)/1.77*dchisq((n-1)*x/1.77, n-1), add=TRUE, col="red") #c'est sigma² ça...

