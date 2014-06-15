#' Sample 1
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
  part <<- cbind(A,B,C,D)
  
  #
  a <- 2
  R3 <<- c(alow=a, aupp=a+1, b=0.5)
}