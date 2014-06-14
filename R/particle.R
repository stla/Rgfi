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
  c(a=ribbon["alow"], b=ribbon["b"], typ=FALSE)
}

#' Extract upper line from a ribbon
#' 
#' @export
Dupp <- function(ribbon){
  c(a=ribbon["aupp"], b=ribbon["b"], typ=TRUE)
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
