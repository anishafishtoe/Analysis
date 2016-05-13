hist(gMean$meanCrossings)
hist(log(gMean$meanCrossings))
hist(sqrt(gMean$meanCrossings))
hist((gMean$meanCrossings)^2)

gud.lm=lm(log(gMean$meanCrossings)~gMean$moonPhase+gMean$nf+gMean$habitat)
summary(gud.lm)
par(mfrow=c(2,2))
plot(gud.lm)
hist(residuals(gud.lm))#not normal!

#interaction
gud.lm.int=lm(log(gMean$meanCrossings)~gMean$moonPhase*gMean$nf*gMean$habitat)
summary(gud.lm.int)
plot(gud.lm.int)
