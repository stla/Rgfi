#' Order a particle
#' 
#' 
#' @export
orderPart <- function(poly){
  # compute an interior point
  O = c( mean(poly[1,1:3]) , mean(poly[2,1:3]) )
  # center the polyhedron around O
  cpoly = poly - O
  # compute the angular parts of polar coordinates
  angles = atan2(cpoly[2,], cpoly[1,])
  # find the order
  ord  = order(angles)
return(poly[,ord])
}

#' Extract lower line from a ribbon
#' 
#' @export
Dlow <- function(ribbon){
  D <- c(ribbon["alow"], ribbon["b"], typ=FALSE)
  names(D)[1:2] <- c("a","b")
  return(D)
}

#' Extract upper line from a ribbon
#' 
#' @export
Dupp <- function(ribbon){
  D <- c(a=ribbon["aupp"], b=ribbon["b"], typ=TRUE)
  names(D)[1:2] <- c("a","b")
  return(D)
}

#' Intersection of two lines
#' 
#' returns the intersection of two lines given by (intercept, slope)
#' 
#' @export
intersect <- function(D1, D2){
  x = (D1[1]-D2[1])/(D2[2]-D1[2])
  return(c(x, D1[1] + D1[2]*x))
}


#' Edge to (intercept, slope)
#' 
#' @export
edge2ab <- function(poly, i){
  poly <- unname(poly)
  A = poly[,i]
  B = poly[,(i %% ncol(poly))+1]
  slope = (B[2]-A[2])/(B[1]-A[1])
  intercept = A[2] - slope*A[1]
return(c(a=intercept, b=slope))
}

#' Create a particle from two ribbons
#' 
#' @export
ipart <- function(R1,R2){
  D11 <- Dlow(R1)
  D12 <- Dupp(R1)
  D21 <- Dlow(R2)
  D22 <- Dupp(R2)
  A <- intersect(D11,D21)
  B <- intersect(D11,D22)
  C <- intersect(D12,D21)
  D <- intersect(D12,D22)
return(orderPart(cbind(A,B,C,D)))
}
