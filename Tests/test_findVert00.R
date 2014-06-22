library(Rgfi)

source('~/Github/Rgfi/Tests/plotPart00.R')

######

sample01() 

findSupport(part, 2, 3)

xlim <- c(0,2)
ylim <- c(0,6.5)
plotPart(part, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)

( poly <- updatePoly(part, Dlow(R3)) )
plotPart(poly, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)
updatePoly(poly, Dupp(R3))

##################

sample02() 
R3
findSupport(part, R3["alow"], R3["aupp"]) # (-2, Inf) or (-Inf, -2) ?? = R!! oui mais on évite peut-être -2 ainsi

R3["b"] <- -5
xlim <- c(-1,0.9)
ylim <- c(0,3.5)
plotPart(part, xlim=xlim, ylim=ylim, R=R3, lines=TRUE, xlab=NA, ylab=NA)

( poly <- updatePoly(part, Dlow(R3)) )
plotPart(poly, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)
( poly <- updatePoly(poly, Dupp(R3)) )
plotPart(poly, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)

##################

sample03() 
R3
findSupport(part, R3["alow"], R3["aupp"])  

R3["b"] <- 2
xlim=c(-1,0.9); ylim=c(0,3.5)
plotPart(part, xlim=xlim, ylim=ylim, R=R3, lines=TRUE, xlab=NA, ylab=NA)
( poly <- updatePoly(part, Dlow(R3)) )
plotPart(poly, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)
( poly <- updatePoly(poly, Dupp(R3)) )
plotPart(poly, xlim=xlim, ylim=ylim, R=R3, lines=TRUE)






