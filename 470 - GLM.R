# load the data
# data is number of seizures patients have before/after taking placebo/drug
# response variable: sumY ~ sum of seizures in eight week peiords
# explanatory variables: 
#     Trt ~ treatment conditions, factors with 2 levels:placebo, drug
#     age ~ age of patients, in years
#     Base ~ number of seizures before taking placebo/drug

library(robust)
data(breslow.dat,package="robust")
str(breslow.dat)
# numerical analysis : show potential skewness problems
summary(breslow.dat[c(6,7,8,10)])
# EDA 
# Why the data is problematic for simple linear regression?
# Ans: SLR request Y and error terms to be normally distributed
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks=20, xlab="Seizure Count",
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")



# Statistical Analysis
# Why choosing Poisson regression/GLM?
# Ans: GLM doesn't require the assumption of normal and equal variance 
#      the context corresponds to Poisson distribution that estimate counts

fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
summary(fit)
# interpretation: 1 year increase in age -> 0.02 increase in log mean of seizures
exp(coef(fit))
# interpretation : 1 year increase in age -> multiplies 1.023 numbers of seizures


# AIC is big, why?
# Ans: Poisson regression assumes mean is equal to variance 
deviance(fit)/df.residual(fit)
# Overdispersion 

fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=quasipoisson())
summary(fit.od)
exp(coef(fit.od))

# other functions
confint(fit.od)
exp(confint(fit.od)) 
residuals(fit.od)
plot(fit.od)



# extensions of changing time periods or calculating rate
breslow.dat$time = 2
fit.t <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, offset= log(time), 
           family=poisson)
# log(mean/time) = ....
summary(fit.t)



# Logistic Regression

# data: a research on number of affairs people have
library(AER)
data(Affairs, package="AER")
str(Affairs)
summary(Affairs)

# decrypt as having or not having affairs
Affairs$ynaffair[Affairs$affairs > 0] <- 1 
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# model building
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation +rating,
                data=Affairs, family=binomial()) 
summary(fit.full)
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, data=Affairs, family=binomial())
summary(fit.reduced)
# check for which model is better
anova(fit.reduced, fit.full, test="Chisq")
# p is not significant, so reduced model is better

coef(fit.reduced)
exp(coef(fit.reduced))


# Overdispersion
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, 
              family = quasibinomial(), data = Affairs)