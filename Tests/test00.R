library(Rgfi)

sample01() 

findSupport(part, 2, 3)

orderPart(part)

plotPart <- function(part, D=NULL, R=NULL, xlim=NULL, ylim=NULL, add=FALSE, edges=TRUE, col.edge="red", vertices=TRUE, lines=FALSE, col.line="black", lty.line="dashed", ...){
  part <- orderPart(part)
  x <- part[1, c(1:ncol(part),1)]
  y <- part[2, c(1:ncol(part),1)]
  #par(tck = 0.02, mgp = c(1.7, 0.3, 0))
  if(is.null(xlim)) xlim <- grDevices::extendrange(x)
  if(is.null(ylim)) ylim <- grDevices::extendrange(y)  
  if(!add) plot(x, y, type = "n", xlim = xlim, ylim = ylim, axes=FALSE, xaxs="i", yaxs="i", ...)
  axis(1, pos=0)
  axis(2, pos=0)
  if(vertices){
    points(rev(x)[-1], rev(y)[-1], pch=19, col=col.edge)
  }
  if(lines){
    if(length(col.line)==1) col.line <- rep(col.line, ncol(part))
    for(i in 1:ncol(part)){
      ab <- edge2ab(part,i)
      abline(a=ab["a"], b=ab["b"], lty=lty.line, col=col.line[i])
    }
  }
  if(edges){
    lines(x, y, col=col.edge, lwd=2)
  }
  if(!is.null(D)){
    abline(a=D["a"], b=D["b"])
  }
  if(!is.null(R)){
    abline(a=Dlow(R)["a"], b=Dlow(R)["b"])
    abline(a=Dupp(R)["a"], b=Dupp(R)["b"])
  }
}

xlim <- c(0,2)
ylim <- c(0,6.5)
plotPart(part, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)

