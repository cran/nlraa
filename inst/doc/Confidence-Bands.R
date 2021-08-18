## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 6, fig.height = 4)

## ----setup2-------------------------------------------------------------------
library(ggplot2)
library(nlraa)
library(nlme)
library(mgcv)

## ---- Oats--------------------------------------------------------------------
data(Oats, package = "nlme")
## A subset for simplicity
Oats.I <- subset(Oats, subset = Block == "I")
plot(Oats.I)

## ---- Oats-fit----------------------------------------------------------------
fm1 <- lm(yield ~ nitro, data = Oats.I)
fm1.prd <- predict(fm1, interval = "conf")
Oats.IA <- cbind(Oats.I, fm1.prd)
## Make a plot
ggplot(data = Oats.IA, aes(x = nitro, y = yield)) + 
  geom_point() + 
  geom_line(aes(y = fit)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "purple", alpha = 0.4)

## ---- Oats-ggplot-geom-smooth-------------------------------------------------
ggplot(data = Oats.IA, aes(x = nitro, y = yield)) + 
  geom_point() + 
  geom_line(aes(y = fit)) + 
  geom_smooth(method = "lm")

## ---- Oats-lm-predict---------------------------------------------------------
fm1.prd.int <- predict(fm1, interval = "pred")
Oats.IAP <- cbind(Oats.I, fm1.prd.int)
## Make a plot
ggplot(data = Oats.IAP, aes(x = nitro, y = yield)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "deepskyblue4", alpha = 0.2) + 
  geom_ribbon(aes(ymin = Oats.IA$lwr, ymax = Oats.IA$upr), fill = "deepskyblue", alpha = 0.6) + 
  geom_line(aes(y = fit), color = "white", size = 1.5) +
  geom_point() + 
  ggtitle("Regression Line, 95% Confidence and Prediction Bands")

## ---- Oats-lm-boot------------------------------------------------------------
fm1.boot <- boot_lm(fm1, fitted)
fm1.boot.prd <- summary_simulate(t(fm1.boot$t))
Oats.IAB <- cbind(Oats.I, fm1.boot.prd)
## Make a plot
ggplot(data = Oats.IAB, aes(x = nitro, y = yield)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "deepskyblue", alpha = 0.6) + 
  geom_line(aes(y = Estimate), color = "white", size = 1.5) +
  geom_point() + 
  ggtitle("95% Bootstrapped Confidence Bands")

## ---- Loblolly----------------------------------------------------------------
Lob <- subset(Loblolly, Seed %in% c("301", "303", "305", "307", "309"))
fnlm1 <- nls(height ~ SSasymp(age, Asym, R0, lrc), data = Lob)
## Plot of observed and fitted
ggplot(Lob, aes(x = age, y = height)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fnlm1)))

## ---- Loblolly-bootstrap-estimates-1, eval = FALSE----------------------------
#  Lob.bt.pe <- boot_nls(fnlm1) ## This takes a few seconds (~7s)

## ---- save-Lob-bt-pe, echo = FALSE, eval = FALSE------------------------------
#  save(Lob.bt.pe, file = "Lob.bt.pe.rda", compress = "xz")

## ---- load-Lob-bt-pe, echo = FALSE--------------------------------------------
data(Lob.bt.pe, package = "nlraa")

## ---- Loblolly-bootstrap-estimate-2-------------------------------------------
pairs(Lob.bt.pe$t, labels = c("Asym", "R0", "lrc"))
print(cov2cor(var(Lob.bt.pe$t)), digits = 2) ## Correlation matrix

## ---- Loblolly-methods--------------------------------------------------------
## Linear model
fm1.Lob <- lm(height ~ poly(age, 3), data = Lob)
fm1.Lob.prd <- predict(fm1.Lob, interval = "conf")
fm1.Lob.dat <- data.frame(method = "lm-poly(3)", Lob, fm1.Lob.prd)
## Nonlinear model + Delta Method
fm2.Lob <- nls(height ~ SSasymp(age, Asym, R0, lrc), data = Lob)
fm2.Lob.dm <- predict2_nls(fm2.Lob, interval = "conf")
fm2.Lob.dm.dat <- data.frame(method = "Delta-Method", Lob,
                             fit = fm2.Lob.dm[,1],
                             lwr = fm2.Lob.dm[,3],
                             upr = fm2.Lob.dm[,4])
## Nonlinear model + bootstrap
fm2.Lob <- nls(height ~ SSasymp(age, Asym, R0, lrc), data = Lob)

## ---- Loblolly-methods-2, eval = FALSE----------------------------------------
#  fm2.Lob.bt <- boot_nls(fm2.Lob, fitted) ## This takes about 7s

## ---- save-fm2-Lob-bt, echo = FALSE, eval = FALSE-----------------------------
#  save(fm2.Lob.bt, file = "fm2.Lob.bt.rda", compress = "xz")

## ---- load-fm2-Lob-bt, echo = FALSE-------------------------------------------
data(fm2.Lob.bt, package = "nlraa")

## ---- Loblolly-methods-3------------------------------------------------------
fm2.Lob.prd <- summary_simulate(t(fm2.Lob.bt$t))
fm2.Lob.bt.dat <- data.frame(method = "nls-bootstrap", Lob, 
                          fit = fm2.Lob.prd[,1], 
                          lwr = fm2.Lob.prd[,3],
                          upr = fm2.Lob.prd[,4])
## Nonlinear model + Monte Carlo
fm2.Lob.MC <- predict_nls(fm2.Lob, interval = "conf")
fm2.Lob.MC.dat <- data.frame(method = "nls-Monte-Carlo", Lob,
                          fit = fm2.Lob.MC[,1], 
                          lwr = fm2.Lob.MC[,3],
                          upr = fm2.Lob.MC[,4])
## GAM
fm3.Lob <- gam(height ~ s(age, k = 3), data = Lob)
fm3.Lob.prd <- predict(fm3.Lob, se.fit = TRUE)
fm3.Lob.GAM.dat <- data.frame(method = "GAM", Lob,
                          fit = fm3.Lob.prd$fit, 
                          lwr = fm3.Lob.prd$fit - 2 * fm3.Lob.prd$se.fit,
                          upr = fm3.Lob.prd$fit + 2 * fm3.Lob.prd$se.fit)
prd.all <- rbind(fm1.Lob.dat, fm2.Lob.dm.dat, fm2.Lob.bt.dat, fm2.Lob.MC.dat, fm3.Lob.GAM.dat)
### Finally plot
ggplot(data = prd.all, aes(x = age, y = height)) + 
  facet_wrap(facets = "method") + 
  geom_line(aes(y = fit)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "purple", alpha = 0.5)

## ---- Puromycin-1-------------------------------------------------------------
PurTrt <- Puromycin[ Puromycin$state == "treated", ]
fm1.P <- nls(rate ~ SSmicmen(conc, Vm, K), data = PurTrt)
## Confidence bands using the Delta method
fm1.P.dm <- predict2_nls(fm1.P, interval = "conf")
## Reproducing book results
round(predict2_nls(fm1.P, interval = "conf", newdata = data.frame(conc = 0.4)), 2)
PurTrtA.dm <- cbind(PurTrt, fm1.P.dm)
ggplot(data = PurTrtA.dm, aes(x = conc, y = rate)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fm1.P))) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "purple", alpha = 0.2) + 
  ggtitle("95% Delta Method Confidence Bands")

## ---- Puromycin-2, eval = FALSE-----------------------------------------------
#  ## Confidence bands using the bootstrap method
#  fm1.P.bt <- boot_nls(fm1.P) ## this takes about 5 seconds

## ---- save-fm1-P-bt, echo = FALSE, eval = FALSE-------------------------------
#  save(fm1.P.bt, file = "fm1.P.bt.rda", compress = "xz")

## ---- load-fm1-P-bt, echo = FALSE---------------------------------------------
data(fm1.P.bt, package = "nlraa")

## ---- Puromycin-3-------------------------------------------------------------
pairs(fm1.P.bt$t, labels = c("Vm", "K"))

## ---- Puromycin-4, eval = FALSE-----------------------------------------------
#  ## Bootstrapped confidence bands
#  fm1.P.bt.ft <- boot_nls(fm1.P, fitted) ## This takes about 5s

## ---- save-fm1-P-bt-ft, echo = FALSE, eval = FALSE----------------------------
#  save(fm1.P.bt.ft, file = "fm1.P.bt.ft.rda", compress = "xz")

## ---- load-fm1-P-bt-ft, echo = FALSE------------------------------------------
data(fm1.P.bt.ft, package = "nlraa")

## ---- Puromycin-5-------------------------------------------------------------
fm1.P.bt.ft.prd <- summary_simulate(t(fm1.P.bt.ft$t))
PurTrtA <- cbind(PurTrt, fm1.P.bt.ft.prd)
## Plot data and bands
ggplot(data = PurTrtA, aes(x = conc, y = rate)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fm1.P))) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "purple", alpha = 0.2) + 
  ggtitle("95% Bootstrap Confidence Bands")
## Predictions at 0.4
prd_fun <- function(x) predict(x, newdata = data.frame(conc = 0.4))
prd_fun(fm1.P) ## It also provides the gradient

## ---- Puromycin-6, eval = FALSE-----------------------------------------------
#  fm1.P.at.x.0.4 <- boot_nls(fm1.P, prd_fun) ## This takes about 6s

## ---- save-fm1-P-at-x-0p4, echo = FALSE, eval = FALSE-------------------------
#  save(fm1.P.at.x.0.4, file = "fm1.P.at.x.0.4.rda", compress = "xz")

## ---- load-fm1-P-at-x-0p4, echo = FALSE---------------------------------------
data(fm1.P.at.x.0.4, package = "nlraa")

## ---- Puromycin-7-------------------------------------------------------------
boot::boot.ci(fm1.P.at.x.0.4, type = "perc") 

## ---- Puromycin-MC------------------------------------------------------------
(prd.at.x.0.4 <- predict_nls(fm1.P, newdata = data.frame(conc = 0.4)))

## ---- Puromycin-prd-----------------------------------------------------------
ndat <- data.frame(conc = seq(0, 1.3, length.out = 50))
Pprd <- predict_nls(fm1.P, interval = "conf",
                    newdata = ndat)
Pprdd <- data.frame(ndat, Pprd)
ggplot() + 
  geom_point(data = PurTrt, aes(x = conc, y = rate)) + 
  geom_line(data = Pprdd, aes(x = conc, y = Estimate)) + 
  geom_ribbon(data = Pprdd, aes(x = conc, ymin = Q2.5, ymax = Q97.5), 
              fill = "purple", alpha = 0.4) + 
  ggtitle("Monte Carlo 95% Confidence Bands")

## ---- maizeleafext------------------------------------------------------------
data(maizeleafext)
## Display the data
fmm1 <- nls(rate ~ SStemp3(temp, t.m, t.l, t.h), data = maizeleafext)
ggplot(data = maizeleafext, aes(x = temp, y = rate)) + 
  geom_point() + geom_line(aes(y = fitted(fmm1)))
## The model seems slightly inadequate for these data
fmm1.dm <- predict2_nls(fmm1, interval = "conf")
mlf <- cbind(maizeleafext, fmm1.dm)
## The confidence bands are fairly wide
ggplot(data = mlf, aes(x = temp, y = rate)) + 
  geom_point() + geom_line(aes(y = fitted(fmm1))) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "purple", alpha = 0.3)

## ---- maizeleafext-2, eval = FALSE--------------------------------------------
#  ## What about bootstrap?
#  fmm1.bt <- boot_nls(fmm1) ## This takes about 5s

## ---- save-fmm1-bt, echo = FALSE, eval = FALSE--------------------------------
#  save(fmm1.bt, file = "fmm1.bt.rda", compress = "xz")

## ---- load-fmm1-bt, echo = FALSE----------------------------------------------
data(fmm1.bt, package = "nlraa")

## ---- maizeleafext-3----------------------------------------------------------
## Notice that the model does not converge in many instances
pairs(fmm1.bt$t, labels = c("t.m", "t.l", "t.h"))

## ---- Theop-------------------------------------------------------------------
data(Theoph)
fmL.Theoph <- nlsList(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = Theoph)
fm0.Theoph <- nlme(fmL.Theoph, random = pdDiag(lKa + lCl ~ 1))
## The Dose is different for each subject however...
ndat <- data.frame(Dose = median(Theoph$Dose), Time = seq(0, 25, by = 0.1))
fm0.Theoph.prd <- predict_nlme(fm0.Theoph, newdata = ndat, interval = "conf")
fm0.Theoph.prd.dat <- cbind(ndat, fm0.Theoph.prd)
## plot data
ggplot() + 
  geom_point(data = Theoph, aes(x = Time, y = conc)) + 
  geom_line(data = fm0.Theoph.prd.dat, aes(x = Time, y = Estimate)) + 
  geom_ribbon(data = fm0.Theoph.prd.dat,
              aes(x = Time, ymin = Q2.5, ymax = Q97.5), fill = "purple", alpha = 0.3) + 
  xlab("Time [h]") + ylab("Theophylline concentration [mg/L]") + 
  ggtitle("95% confidence bands")

## ---- Theop-PB-within-subject-------------------------------------------------
fm0.Theoph.prd.bnd <- predict_nlme(fm0.Theoph, plevel = 1, interval = "pred")
fm0.Theoph.prd.bnd.dat <- cbind(Theoph, fm0.Theoph.prd.bnd)
## Plot it
ggplot(data = fm0.Theoph.prd.bnd.dat, aes(x = Time, y = conc)) +
  facet_wrap(~Subject) + 
  geom_point() + 
  geom_line(aes(x = Time, y = Estimate)) + 
    geom_ribbon(data = fm0.Theoph.prd.bnd.dat,
              aes(x = Time, ymin = Q2.5, ymax = Q97.5), fill = "purple", alpha = 0.3) + 
  xlab("Time [h]") + ylab("Theophylline concentration [mg/L]") + 
  ggtitle("95% prediction bands (within subjects)")

## ---- Theop-PB-between-subjects-----------------------------------------------
ndat <- expand.grid(Dose = median(Theoph$Dose), Time = 0:25, Subject = unique(Theoph$Subject))
ndat$Estimate <- predict(fm0.Theoph, newdata = ndat, level = 1)
fm0.Theoph.simA <- ndat
## Plot the simulations
ggplot() + 
  geom_point(data = Theoph, aes(x = Time, y = conc)) + 
  geom_line(data = fm0.Theoph.simA, aes(x = Time, y = Estimate, group = Subject),
            color = "gray") + 
  geom_ribbon()

## ---- Theoph-PB-between-subjects, eval = FALSE--------------------------------
#  pred_band_BS <- function(x) predict(x, newdata = ndat, level = 1)
#  fm0.Theoph.bt <- boot_nlme(fm0.Theoph, pred_band_BS) ## This takes a bit over a minute

## ---- Theoph-PB-between-subjects-3, eval = FALSE------------------------------
#  fm0.Theoph.bt.ss <- cbind(ndat[,-4], summary_simulate(t(na.omit(fm0.Theoph.bt$t))))
#  fm0.Theoph.bt.ss.A <- aggregate(cbind(Estimate, Est.Error, Q2.5, Q97.5) ~ Time,
#                                  data = fm0.Theoph.bt.ss, FUN = mean)
#  ## plot data
#  ggplot() +
#    geom_point(data = Theoph, aes(x = Time, y = conc)) +
#    geom_line(data = fm0.Theoph.bt.ss.A, aes(x = Time, y = Estimate)) +
#    geom_ribbon(data = fm0.Theoph.bt.ss.A, aes(x = Time, ymin = Q2.5, ymax = Q97.5), fill = "purple", alpha = 0.3) +
#    xlab("Time [h]") + ylab("Theophylline concentration [mg/L]") +
#    ggtitle("95% prediction bands (between subjects)")

