
findSupport <- function(poly, l, u){
	sigma <- poly[1,]
	VTsum <- poly[2,]
  if((min(sigma) > 0) || (max(sigma)<0)){ # 1er cas : la cellule ne contient pas sigma=0 (tous les sigma de même signe)
    return(range(-c((l-VTsum)/sigma, (u-VTsum)/sigma)))
  }else{ # 2ème cas : l'axe des VTsum coupe la cellule 
    where <- c(isInside(poly, l), isInside(poly, u))
    #cas 2a : un nouveau point (l ou u) à l'intérieur
    if(where[1]=="inside" || where[2]=="inside"){ 
      return(c(m=-Inf, M=Inf))
    }
	
    #cas 2b : les 2 points au-dessus ? ce n'est pas un intervalle
    if(where[1]=="above"){
      #
      neg <- which(sigma<0)
      left <- min(c((VTsum[neg]-l)/sigma[neg]))
      pleft <- 1-pcauchy(left)
      # 
      pos <- which(sigma>0)
      right <- max(c((VTsum[pos]-l)/sigma[pos])) # ou (VTsum[i]-l)/sigma[i] avec i <- which.max(sigma)
      pright <- pcauchy(right)
      #
      p <- pleft+pright
      if(runif(1)<pleft/p) return(c(left,Inf))
      else return(c(-Inf,right))
    }
    #cas 2c : les 2 points en-dessous
    if(where[1]=="below"){
      #
      neg <- which(sigma<0)
      left <- max(c((VTsum[neg]-u)/sigma[neg])) # plutôt u!!
      pleft <- pcauchy(left)
      # 
      pos <- which(sigma>0)
      right <- min(c((VTsum[pos]-u)/sigma[pos])) 
      pright <- 1-pcauchy(right)
      #
      p <- pleft+pright
      if(runif(1)<pleft/p) return(c(-Inf,left))
      else return(c(right,Inf))
    }
    #cas 2c : les 2 points autour
    if(where[1]=="below" && where[2]=="above"){# lower is below, upper is above
                    return("nothing to do")
	}
  }
}

isInside <- function(opoly, a){
        x1 = opoly[1,]
        x2 = x1[c(2:length(x1), 1)]
        # ceux qui coupent
        cutting = which( x1*x2 < 0 )
        #
		intercepts <- rep(NA,2)
		for(i in 1:2){
			A = opoly[,cutting[i]]
			B = opoly[,(cutting[i] %% ncol(opoly))+1]
			intercepts[i] <- A[2] - (B[2]-A[2])/(B[1]-A[1])*A[1]
		}
		amin <- min(intercepts)
		amax <- max(intercepts)
        #
        if(a < amin) return("below")
        else if(a > amax) return("above")
        else return("inside")
}

