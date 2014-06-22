library(Rgfi)

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
    points(c(0,0), c(R["alow"], R["aupp"]), pch=19, col="blue")
  }
}


######

sample01() 

findSupport(part, 2, 3)

orderPart(part)


xlim <- c(0,2)
ylim <- c(0,6.5)
plotPart(part, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)

##################

sample02() 
R3
findSupport(part, R3["alow"], R3["aupp"]) # (-2, Inf) or (-Inf, -2) ?? = R!! oui mais on évite peut-être -2 ainsi
orderPart(part) 

nsims <- 10000
sims <- rep(NA,nsims)
for(i in 1:nsims){
  sims[i] <- findSupport(part, R3["alow"], R3["aupp"])[1]
}
table(sims)
table(sims)[1]/nsims
pcauchy(-2)


R3["b"] <- -5
plotPart(part, xlim=c(-1,0.9), ylim=c(0,3.5), R=R3, lines=TRUE, xlab=NA, ylab=NA)

##################

sample03() 
R3
findSupport(part, R3["alow"], R3["aupp"]) # (-0.6, Inf) or (-Inf, -2.4375) 
orderPart(part) 

R3["b"] <- -0.6
plotPart(part, xlim=c(-1,0.9), ylim=c(0,3.5), R=R3, lines=TRUE, xlab=NA, ylab=NA)
R3["b"] <- -2.4375
plotPart(part, xlim=c(-1,0.9), ylim=c(0,3.5), R=R3, lines=TRUE, xlab=NA, ylab=NA)


nsims <- 10000
sims <- rep(NA,nsims)
for(i in 1:nsims){
  sims[i] <- findSupport(part, R3["alow"], R3["aupp"])[1]
}
table(sims)
table(sims)[1]/nsims
pcauchy(-2.4375)/(pcauchy(-2.4375)+(1-pcauchy(-0.6)))


