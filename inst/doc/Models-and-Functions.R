## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(nlme)
library(nlraa)
library(ggplot2)
library(knitr)

## ----lm-1---------------------------------------------------------------------
x <- rnorm(20)
y <- 1 + 2 * x + rnorm(20, sd = 0.5)
plot(x, y)
fit <- lm(y ~ x)

## ----lm-coef------------------------------------------------------------------
## Print beta hat
coef(fit)
## Print sigma
sigma(fit)
## Print the covariance matrix of beta hat
vcov(fit)

## ----lm-var_cov---------------------------------------------------------------
## Extract the variance covariance of the residuals (which is also of the response)
lm.vc <- var_cov(fit)
## Print just the first 5 observations
round(lm.vc[1:5,1:5],2)
## Visualize the matrix. This is a 20 x 20 matrix.
## It is easier to visualize in the log-scale
## Note: log(0) becomes -Inf, but not shown here
image(log(lm.vc[,ncol(lm.vc):1]))

## ----lm-table, echo = FALSE---------------------------------------------------
dat <- data.frame(r.function = "lm", beta = "coef", 
                  cov.beta = "vcov", sigma = "sigma",
                  X = "model.matrix", y.hat = "fitted",
                  e = "residuals", cov.e = "var_cov")
kable(dat)

## ----gls-chickweight----------------------------------------------------------
## ChickWeight example
data(ChickWeight)
ggplot(data = ChickWeight, aes(Time, weight)) + geom_point()

## ----gls-chickweight-weights--------------------------------------------------
## One possible model is
fit.gls <- gls(weight ~ Time, data = ChickWeight,
               weights = varPower())
fit.gls.vc <- var_cov(fit.gls)
## Note: the function getVarCov fails for this object
## Visualize the first few observations
fit.gls.vc[1:10,1:10]
## Store for visualization
## only the first 12 observations
vc2 <- fit.gls.vc[1:12,1:12]
round(vc2,0)
## The variance increases as weight increases
## Visualize the variance-covariance of the errors
## This is only for the first 12 observations
## It is easier to visualize in the log scale
## Note: log(0) becomes -Inf, but not shown here
image(log(vc2[,ncol(vc2):1]), 
      main = "First 12 observations, Cov(resid)")
## For all observations
image(log(fit.gls.vc[,ncol(fit.gls.vc):1]), 
      main = "All observations, Cov(resid)")

## ----gls-chickweight-weights-corr---------------------------------------------
## Adding the correlation
fit.gls2 <- gls(weight ~ Time, data = ChickWeight,
                weights = varPower(), 
                correlation = corCAR1(form = ~ Time | Chick))
## Extract the variance-covariance of the residuals
## Note: getVarCov fails for this object
fit.gls2.vc <- var_cov(fit.gls2)
## Visualize the first few observations
round(fit.gls2.vc[1:13,1:13],0)
## Visualize the variance-covariance of the errors
## On the log-scale
## Reorder and select
vc2.36 <- fit.gls2.vc[1:36,1:36]
image(log(vc2.36[,ncol(vc2.36):1]),
      main = "Covariance matrix of residuals \n for the first three Chicks (log-scale)")

## ----ChickWeight-ggplot2-facet------------------------------------------------
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  facet_wrap( ~ Chick) + 
  geom_point()

