\name{biserial.cor}

\alias{biserial.cor}

\title{

    Point-Biserial Correlation

}

\description{

  Computes the point-biserial correlation between a dichotomous and a continuous variable.

}

\usage{

biserial.cor(x, y, use = c("all.obs", "complete.obs"), level = 1)

}

\arguments{
  \item{x}{a numeric vector representing the continuous variable.}
  \item{y}{a factor or a numeric vector (that will be converted to a factor) representing the dichotomous variable.}
  \item{use}{If \code{use} is "all.obs", then the presence of missing observations will produce an error. If \code{use}
    is "complete.obs" then missing values are handled by casewise deletion.}
  \item{level}{which level of \code{y} to use.}
}

\details{ 
    The point biserial correlation computed by \code{biserial.cor()} is defined as follows \deqn{r = 
    \frac{(\overline{X}_1 - \overline{X}_0)\sqrt{\pi (1 - \pi)}}{S_x},}{(X1.bar - X0.bar) * sqrt(pi * (1 - pi)) / S_x,} 
    where \eqn{\overline{X}_1}{X1.bar} and \eqn{\overline{X}_0}{X0.bar} denote the sample means of the \eqn{X}-values 
    corresponding to the first and second level of \eqn{Y}, respectively, \eqn{S_x} is the sample standard deviation of
    \eqn{X}, and \eqn{\pi}{pi} is the sample proportion for \eqn{Y = 1}. The first level of \eqn{Y} is defined by the
    \code{level} argument; see \bold{Examples}. 
}


\note{

Changing the order of the levels for \code{y} will produce a different result. By default, the first level is used
as a reference level

}

\value{

the (numeric) value of the point-biserial correlation.

}

\author{

    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}

}


\examples{

# the point-biserial correlation between
# the total score and the first item, using
# '0' as the reference level
biserial.cor(rowSums(LSAT), LSAT[[1]])

# and using '1' as the reference level
biserial.cor(rowSums(LSAT), LSAT[[1]], level = 2)

}

\keyword{ multivariate }
