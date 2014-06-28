#' Find intersection - case 1
#' 
#' @export

# case one edge to remove
updatePoly1 <- function(opoly, D, toRemove){
  # first edge
  index = if(toRemove==1) ncol(opoly) else toRemove-1 
  M = intersect(c(D["a"],D["b"]), edge2ab(opoly,index))
  # second edge
  index = if(toRemove==ncol(opoly)) 1 else toRemove+1 
  N = intersect(c(D["a"],D["b"]), edge2ab(opoly,index))
  #
  opoly[,toRemove] = M
  opoly[,index] = N
  #
  return(opoly)
}

#' Find intersection - case 2
#' 
#' @export

# case "chanfrein"
updatePoly2 <- function(opoly, D, Dinters, test1){
  # shift to put the two edges at first positions
  ncol = ncol(opoly)
  if(Dinters[2]-Dinters[1] != 1){
    arrange = c(ncol, 1:(ncol-1))
  }else{
    arrange = (((1:ncol)+Dinters[1]-2) %% ncol)+1
  }
  opoly = opoly[, arrange]
  # M
  M = intersect(c(D["a"],D["b"]), edge2ab(opoly,1))
  # N
  N = intersect(c(D["a"],D["b"]), edge2ab(opoly,2))
  #
  test = test1[arrange][2] # test2[1]
  if( (!D["typ"] && !test) || (D["typ"] && test) ){
    return(cbind(opoly[,1], M, N, opoly[, 3:ncol]))
  } else{
    return(cbind(M, opoly[,2], N))
  }
}

#' Find intersection - general case
#' 
#' @export

updatePoly  <- function(opoly, D){ #
  x1 = opoly[1,]
  y1 = opoly[2,]
  #x2 = x1[c(2:length(x1), 1)]
  #y2 = y1[c(2:length(x1), 1)]
  test1 = y1 > (D["a"] + D["b"] * x1)
  test2 = test1[c(2:length(x1), 1)] #y2 > (D["a"] + D["b"] * x2) # c'est simplement test1 shifté
  test = test1 + test2
  if(D["typ"]==FALSE){
    Remove <- test == 0
  } else{
    Remove <- test == 2
  }
  toRemove = which(Remove)
  if(length(toRemove) == 1){
    #print("case 1\n")
    return(updatePoly1(opoly, D, toRemove[1]))
  } else if(length(toRemove) == 0){
    Dinters = which(test == 1)
    if(length(Dinters) == 2){
      #print("case 2\n")
      return(updatePoly2(opoly, D, Dinters, test1))
    } else{
      #print("nothing to do")
      return(opoly)
    }
  } else if(Remove[1] && Remove[length(Remove)]){
    indices = which(!Remove)
    torem =  length(indices)+1
    indices = c(indices, indices[length(indices)]+1)
  } else{
    indices = 1:ncol(opoly)
    indices <- indices[-(toRemove[2]:toRemove[length(toRemove)])]
    torem = toRemove[1]
  }
  #print("case 3\n")
  updatePoly1(opoly[,indices], D, torem)
}
