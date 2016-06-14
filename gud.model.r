#mixed effects models
library(lme4)
library(car)
library(MASS)


#Are GUDS normal?
hist(gMean$meanGUD, main="", xlab="Mean GUD")
hist(log(gMean$meanGUD))
#main effects, no interaction

microhabitat=lmer(meanGUD~FeedingTrayPosition+(1|fStn), REML=FALSE, data=gMean, family="poisson")
habitat=lmer(meanGUD~habitat+(1|fStn), REML=FALSE, data=gMean)
moonPhase=lmer(meanGUD~moonPhase+(1|fStn), REML=FALSE, data=gMean)
month=lmer(meanGUD~month+(1|fStn), REML=FALSE, data=gMean)


#Full model and significance testing for fixed effects
full.model=lmer(meanGUD~FeedingTrayPosition+habitat+moonPhase+month+FeedingTrayPosition*habitat+FeedingTrayPosition*moonPhase+moonPhase*habitat +habitat*month +FeedingTrayPosition*habitat*month+habitat*moonPhase*month+FeedingTrayPosition*moonPhase*habitat+(1|fStn), data=gMean, REML=FALSE)
Anova(full.model)

#GLMM with Laplace
summary(glmer(meanGUD~FeedingTrayPosition+habitat+moonPhase+month+FeedingTrayPosition*habitat+
                FeedingTrayPosition*moonPhase+moonPhase*habitat +habitat*month +
                FeedingTrayPosition*habitat*month+habitat*moonPhase*month+
                FeedingTrayPosition*moonPhase*habitat+(1|fStn), data= gMean, family= Gamma(link="log")))

#0906
gud.lmer.fullmodel=lmer(data=gMean, log10(meanGUD+1)~ moonPhase*habitat*FeedingTrayPosition*month +(1|fStn), REML=FALSE)
plot(gud.lmer.fullmodel)

res= residuals(gud.lmer.fullmodel)
fit= fitted(gud.lmer.fullmodel)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
plot(x=fit, y=res, xlab="Fitted", ylab="Residuals")
boxplot(res~moonPhase, data= gMean, ylab="Residuals")
boxplot(res~habitat, data= gMean, ylab="Residuals")
boxplot(res~FeedingTrayPosition, data= gMean, ylab="Residuals")
boxplot(res~month, data= gMean, ylab="Residuals")

Anova(gud.lmer.fullmodel)

gud.sigmodel=lmer(data=gMean, log10(meanGUD+1)~month+habitat*month+(1|fStn), REML=FALSE)
anova(gud.lmer.fullmodel, gud.sigmodel)

gud.lmer.full.month=lmer(data=subset(gMean, month =="Month2"), meanGUD~ moonPhase*habitat*FeedingTrayPosition +(1|fStn), REML=FALSE)
plot(gud.lmer.fullmodel)
Anova(gud.lmer.full.month)
