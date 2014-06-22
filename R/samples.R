#' Sample 1
#' 
#' @details
#' A simple situation for the range
#' 
#' @export
sample01 <- function(){
  D11 <- c(
    a=0.4,
    b=1.5,
    type=FALSE
  )
  D12 <- c(
    a=1.5, 
    b=1.5, # must be equal  to D11
    type=TRUE
  )
  D21 <- c(
    a=4.5,
    b=-2,
    type=FALSE
  )
  D22 <- c(
    a=5.9,
    b=-2,
    type=TRUE
  )
  A <- intersect(D11,D21)
  B <- intersect(D11,D22)
  C <- intersect(D12,D21)
  D <- intersect(D12,D22)
  part <<- orderPart(cbind(A,B,C,D))
  
  #
  a <- 2
  R3 <<- c(alow=a, aupp=a+1, b=0.5)
}

#' Sample 2
#' 
#' @details
#' One new intercept is on one edge of the particle
#' 
#' @export
sample02 <- function(){
  R1 <<- c(alow=0.4, aupp=2.5, b=1.5)
  D11 <- Dlow(R1)
  D12 <- Dupp(R1)
  R2 <<- c(alow=1, aupp=2, b=-2)
  D21 <- Dlow(R2)
  D22 <- Dupp(R2)
  A <- intersect(D11,D21)
  B <- intersect(D11,D22)
  C <- intersect(D12,D21)
  D <- intersect(D12,D22)
  part <<- orderPart(cbind(A,B,C,D))
  R3 <<- c(alow=2, aupp=3, b=0.5)
}

#' Sample 3
#' 
#' @details
#' Two intercepts above the particle
#' 
#' @export
sample03 <- function(){
  R1 <- c(alow=0.4, aupp=2.5, b=1.5)
  D11 <- Dlow(R1)
  D12 <- Dupp(R1)
  R2 <<- c(alow=1, aupp=2, b=-2)
  D21 <- Dlow(R2)
  D22 <- Dupp(R2)
  A <- intersect(D11,D21)
  B <- intersect(D11,D22)
  C <- intersect(D12,D21)
  D <- intersect(D12,D22)
  part <<- orderPart(cbind(A,B,C,D))
  R3 <<- c(alow=2.2, aupp=3, b=0.5)
}