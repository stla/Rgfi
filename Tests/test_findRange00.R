library(Rgfi)

source('~/Github/Rgfi/Tests/plotPart00.R')

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


