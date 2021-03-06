% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SShill.R
\name{SShill}
\alias{SShill}
\alias{hill1}
\alias{SShill1}
\alias{hill2}
\alias{SShill2}
\alias{hill3}
\alias{SShill3}
\title{self start for Hill Function}
\usage{
hill1(x, Ka)

SShill1(x, Ka)

hill2(x, Ka, n)

SShill2(x, Ka, n)

hill3(x, Ka, n, a)

SShill3(x, Ka, n, a)
}
\arguments{
\item{x}{input vector (x). Concentration of substrate in the original Hill model.}

\item{Ka}{parameter representing the concentration at which half of maximum y is attained}

\item{n}{parameter which controls the curvature}

\item{a}{parameter which controls the maximum value of the response (asymptote)}
}
\value{
hill1: vector of the same length as x (time) using the Hill 1 function

hill2: vector of the same length as x (time) using the Hill 2 function

hill3: vector of the same length as x (time) using the Hill 3 function
}
\description{
Self starter for Hill function with parameters Ka, n and a
}
\details{
For details see https://en.wikipedia.org/wiki/Hill_equation_(biochemistry)

The form of the equations are: \cr
hill1: \deqn{1 / (1 + (Ka/x))}. \cr 
hill2: \deqn{1 / (1 + (Ka/x)^n)}. \cr
hill3: \deqn{a / (1 + (Ka/x)^n)}. \cr
}
\note{
Zero values are not allowed.
}
\examples{
\donttest{
require(ggplot2)
## Example for hill1
set.seed(1234)
x <- 1:20
y <- hill1(x, 10) + rnorm(20, sd = 0.03)
dat1 <- data.frame(x = x, y = y)
fit1 <- nls(y ~ SShill1(x, Ka), data = dat1)

## Example for hill2
y <- hill2(x, 10, 1.5) + rnorm(20, sd = 0.03)
dat2 <- data.frame(x = x, y = y)
fit2 <- nls(y ~ SShill2(x, Ka, n), data = dat2)

## Example for hill3
y <- hill3(x, 10, 1.5, 5) + rnorm(20, sd = 0.03)
dat3 <- data.frame(x = x, y = y)
fit3 <- nls(y ~ SShill3(x, Ka, n, a), data = dat3)

ggplot(data = dat3, aes(x, y)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit3)))
}
}
