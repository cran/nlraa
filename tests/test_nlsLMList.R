## Testing the nlsLMList with functionality

require(nlraa)

data(barley, package = "nlraa")

barleyG <- nlme::groupedData(yield ~ NF | year, data = barley)
fitL <- nlsLMList(yield ~ SSlinp(NF, a, b, xs), data = barleyG)

## functionality for lower and upper
fitL1 <- nlsLMList(yield ~ SSlinp(NF, a, b, xs), data = barleyG,
                   lower = c(0, 0, 7))

fitL2 <- nlsLMList(yield ~ SSlinp(NF, a, b, xs), data = barleyG,
                   upper = c(200, 100, 20))

fitL1 <- nlsLMList(yield ~ SSlinp(NF, a, b, xs), data = barleyG,
                   lower = c(0, 0, 0),  upper = c(200, 100, 20),
                   algorithm = "port")

fitL2 <- nlsLMList(yield ~ SSlinp(NF, a, b, xs), data = barleyG,
                   upper = c(200, 100, 20))

## Test from SO
## https://stackoverflow.com/questions/74033303/use-of-algorithm-port-and-control-lower-limit-in-nlslist#74036103
# dat<-read.table(text="time gluc starch solka
# 1 6.32 7.51 1.95
# 2 20.11 25.49 6.43
# 3 36.03 47.53 10.39
# 6 107.52 166.31 27.01
# 12 259.28 305.19 113.72
# 24 283.40 342.56 251.14
# 48 297.55 353.66 314.22", header = TRUE)
# long <- tidyr::pivot_longer(dat, -1, values_to = "y")
# long$name <- factor(long$name)
# st0 <- list(Max = 200, k = 0.1, Lag = 0.5)
# nlsLMList(y ~ (time > Lag) * Max * (1-exp(-k * (time - Lag))) | name,
#         long,
#         algorithm="port",
#         lower=c(Max =-Inf, k = 0.1, Lag = -Inf),
#         start = st0)

# library(ggplot2)
# 
# ggplot(data = long, aes(x = time, y = y, color = name)) + 
#   geom_point()
# 
# longG <- groupedData(y ~ time | name, data = long)
# 
# fitL <- nlsLMList(y ~ (time > Lag) * Max * (1-exp(-k * (time - Lag))),
#                   longG,
#                   start = st0)
# 
# fm <- nlme(fitL, random = pdDiag(Lag + Max + k ~ 1))
# 
# plot(augPred(fm, level = 0:1))
# 
# fgm <- gnls(y ~ (time > Lag) * Max * (1-exp(-k * (time - Lag))),
#             data = long,
#             params = Lag + Max + k ~ name,
#             start = c(Lag = 2.4, 0, 0, Max = 336, 0, 0, k = 0.12, 0, 0))


