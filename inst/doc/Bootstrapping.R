## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 6, fig.height = 4)

## ----setup2--------------------------------------------------------------
library(ggplot2)
library(nlraa)
library(car)
library(nlme)

## ----barley--------------------------------------------------------------
data(barley, package = "nlraa")

## Quick visualization
ggplot(data = barley, aes(x = NF, y = yield)) + geom_point()

## ----linp----------------------------------------------------------------
## Linear-plateau model
## The function SSlinp is in the 'nlraa' package
fit.lp <- nls(yield ~ SSlinp(NF, a, b, xs), data = barley)

## Visualize data and fit
ggplot(data = barley, aes(x = NF, y = yield)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit.lp)))

## ----confint-fit-lp------------------------------------------------------
confint(fit.lp)

## ----summary-fit-lp------------------------------------------------------
summary(fit.lp)

## ----plot-profile--------------------------------------------------------
## For the intercept
plot(profile(fit.lp, "a"))
## This one is fairly symetric and the normal approximation is reasonable
plot(profile(fit.lp, "b"))
plot(profile(fit.lp, "xs"))
## These last parameters are less symetrical

## ----barley-Boot---------------------------------------------------------
fit.lp.Bt <- Boot(fit.lp)

## ----barley-Boot-asymp---------------------------------------------------
fn <- function(x) coef(x)[1] + coef(x)[2] * coef(x)[3]
fit.lp.Bt.asymp <- Boot(fit.lp, f = fn, labels = "asymptote")
confint(fit.lp.Bt.asymp)
hist(fit.lp.Bt.asymp)

## ----barley-Boot-deltaMethod---------------------------------------------
fit.lp.Dlt.asymp <- deltaMethod(fit.lp, "a + b * xs")
fit.lp.Dlt.asymp

## ----fit-lp-pred-uncertainty---------------------------------------------
## The object 't' in the bootstrap run has 
## the parameter estimate values
## First remove missing values
fit.lp.Bt.prms <- na.omit(fit.lp.Bt$t)

nrb <- length(unique(barley$NF))
nrp <- nrow(fit.lp.Bt.prms)

## Set up an empty data.frame  
prd.dat <- data.frame(i = as.factor(rep(1:nrp, each = nrb)), NF = rep(unique(barley$NF), nrp), prd = NA)

## A simple loop can be used to run the model multiple times
for(i in 1:nrp){
  a.i <- fit.lp.Bt.prms[i,1]
  b.i <- fit.lp.Bt.prms[i,2]
  xs.i <- fit.lp.Bt.prms[i,3]
  
  prd.dat[c(1 + (nrb*(i - 1))):c(i * nrb),3] <- linp(unique(barley$NF), a.i, b.i, xs.i)
}

## Plot the data with the original fit and the uncertainty
ggplot() + 
  geom_line(data = prd.dat, aes(x = NF, y = prd, group = i), 
            color = "gray", alpha = 0.2) +
  geom_line(data = barley, aes(x = NF, y = fitted(fit.lp))) + 
  geom_point(data = barley, aes(x = NF, y = yield)) + 
  ylab("Yield") + xlab("Nitrogen rate") + 
  ggtitle("Using results from Boot \n and plug-in into linp")

## ----fit-lp-Boot-uncertainty-2-------------------------------------------
fn2 <- function(x) predict(x, newdata = data.frame(NF = 0:14))
fit.lp.Bt2 <- Boot(fit.lp, fn2)
fttd <- na.omit(fit.lp.Bt2$t)
prds <- c(t(fttd))
ndat <- data.frame(i = as.factor(rep(1:nrow(fttd), each = ncol(fttd))),
                   NF = rep(0:14, nrow(fttd)))
ndat$prd <- prds

## Essentially the same graph as the one above
ggplot() + 
  geom_line(data = ndat, aes(x = NF, y = prd, group = i), 
            color = "gray", alpha = 0.2) + 
  geom_line(data = barley, aes(x = NF, y = fitted(fit.lp))) + 
  geom_point(data = barley, aes(x = NF, y = yield)) + 
  ylab("Yield") + xlab("Nitrogen rate")

## ----barley-gls2---------------------------------------------------------
set.seed(101)
## Simplify the dataset to make the set up simpler
barley2 <- subset(barley, year < 1974)

fit.lp.gnls2 <- gnls(yield ~ SSlinp(NF, a, b, xs), data = barley2)

intervals(fit.lp.gnls2)

## Compare this to the bootstrapping approach
fit.lp.gnls2.bt <- boot_nlme(fit.lp.gnls2, R = 200)

summary(fit.lp.gnls2.bt)

confint(fit.lp.gnls2.bt, type = "perc")

## ----gnls-factors--------------------------------------------------------
set.seed(101)
barley2$year.f <- as.factor(barley2$year)

cfs <- coef(fit.lp.gnls2)

fit.lp.gnls3 <- update(fit.lp.gnls2, 
                      params = list(a + b + xs ~ year.f),
                      start = c(cfs[1], 0, 0, 0, 
                                cfs[2], 0, 0, 0,
                                cfs[3], 0, 0, 0))

## This bootstraps the vector of parameters
fit.lp.gnls3.bt <- boot_nlme(fit.lp.gnls3, R = 300)

confint(fit.lp.gnls3.bt, type = "perc")

hist(fit.lp.gnls3.bt, 1, ci = "perc")

## ----barley-nlme---------------------------------------------------------
set.seed(101)
barley$year.f <- as.factor(barley$year)

barleyG <- groupedData(yield ~ NF | year.f, data = barley)

fitL.bar <- nlsList(yield ~ SSlinp(NF, a, b, xs), data = barleyG)

fit.bar.nlme <- nlme(fitL.bar, random = pdDiag(a + b + xs ~ 1))

## Confidence intervals of the model fixed parameters
intervals(fit.bar.nlme, which = "fixed")

## Function which computes the asymptote
fna <- function(x) fixef(x)[1] + fixef(x)[2] * fixef(x)[3]

## Bootstrap the model for the asymptote
fit.bar.nlme.bt <- boot_nlme(fit.bar.nlme, f = fna, R = 200)

confint(fit.bar.nlme.bt, type = "perc")

hist(fit.bar.nlme.bt, ci = "perc")

## ----Orange--------------------------------------------------------------
data(Orange)

## This fits a model which considers the fact that 
## the variance typically increases as the fitted
## values increase
fitg <- gnls(circumference ~ SSlogis(age, Asym, xmid, scal), 
              data = Orange, weights = varPower())

## Here we use bootstrapping to investigate 
## the uncertainty around the fitted values
fitg.bt1 <- boot_nlme(fitg, fitted, psim = 1, R = 300)
  
## Compute 90% quantile confidence bands
lwr1.q <- apply(t(fitg.bt1$t), 1, quantile, probs = 0.05, na.rm = TRUE)
upr1.q <- apply(t(fitg.bt1$t), 1, quantile, probs = 0.95, na.rm = TRUE)

ggplot() + 
  geom_point(data = Orange, aes(x = age, y = circumference)) + 
  geom_line(data = Orange, aes(x = age, y = fitted(fitg))) + 
  geom_ribbon(aes(x = Orange$age, ymin = lwr1.q, ymax = upr1.q), 
                fill = "purple", alpha = 0.2) + 
  ggtitle("Orange fit using the logistic: \n 90% confidence band for the mean function")

## ----Orange-psim-0-level-0-----------------------------------------------
fmoL <- nlsList(circumference ~ SSlogis(age, Asym, xmid, scal), data = Orange)

fmo <- nlme(fmoL, random = pdDiag(Asym + xmid + scal ~ 1))

## Just one simulation, because with psim = 0 and level = 0, we are 
## computing the predicted values at level = 0 (?predict.nlme)
sim00 <- simulate_nlme(fmo, nsim = 1, psim = 0, level = 0)

dat00 <- cbind(Orange, prd = as.vector(sim00))

ggplot(data = dat00) + 
  geom_point(aes(x = age, y = circumference)) + 
  geom_line(aes(x = age, y = prd)) + 
  ggtitle("psim = 0, level = 0")

## ----Orange-psim-1-level-0-----------------------------------------------
sdat10 <- simulate_nlme(fmo, nsim = 100, psim = 1, level = 0, value = "data.frame")

ggplot(data = sdat10) + 
  geom_line(aes(x = age, y = y.sim, group = ii), color = "gray", alpha = 0.5) + 
  geom_point(aes(x = age, y = circumference)) + 
  ggtitle("psim = 1, level = 0") + 
  ylab("circumference")

## ----Orange-psim-1-level-1-----------------------------------------------
sdat11 <- simulate_nlme(fmo, nsim = 100, psim = 1, level = 1, value = "data.frame")

## We need a tree simulation ID
## for plotting
sdat11$Tree_ID <- with(sdat11, paste0(Tree,"_",ii))

ggplot(data = sdat11) + 
  facet_wrap(~ Tree) + 
  geom_line(aes(x = age, y = y.sim, color = Tree, group = Tree_ID), 
            alpha = 0.5) + 
  geom_point(aes(x = age, y = circumference)) + 
  ggtitle("psim = 1, level = 1") + 
  ylab("circumference") + 
  theme(legend.position = "none")

## ----Orange-psim-2-level-1-----------------------------------------------
sdat21 <- simulate_nlme(fmo, nsim = 100, psim = 2, level = 1, value = "data.frame")

## Here I'm plotting points to emphasize that we are making
## predictions at the level of a single observation
ggplot(data = sdat21) + 
  facet_wrap(~ Tree) + 
  geom_point(aes(x = age, y = y.sim, color = Tree), 
            alpha = 0.5) + 
  geom_point(aes(x = age, y = circumference)) + 
  ggtitle("psim = 2, level = 1") + 
  ylab("circumference") + 
  theme(legend.position = "none")

