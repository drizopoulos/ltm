\name{GoF}
\alias{GoF.gpcm}
\alias{GoF.rasch}

\title{ Goodness of Fit for Rasch Models}

\description{
  Performs a parametric Bootstrap test for Rasch and Generalized Partial Credit models. 
}

\usage{
GoF.gpcm(object, simulate.p.value = TRUE, B = 99, seed = NULL, \dots)

GoF.rasch(object, B = 49, \dots)
}

\arguments{
  \item{object}{an object inheriting from either class \code{gpcm} or class \code{rasch}.}
  \item{simulate.p.value}{logical; if \code{TRUE}, the reported \eqn{p}-value is based on a parametric Bootstrap approach.
        Otherwise the \eqn{p}-value is based on the asymptotic chi-squared distribution.}
  \item{B}{the number of Bootstrap samples. See \bold{Details} section for more info.}
  \item{seed}{the seed to be used during the parametric Bootstrap; if \code{NULL}, a random seed is used.}
  \item{\dots}{additional arguments; currently none is used.}
}

\details{
  \code{GoF.gpcm} and \code{GoF.rasch} perform a parametric Bootstrap test based on Pearson's chi-squared statistic defined as
  \deqn{\sum\limits_{r = 1}^{2^p} \frac{\{O(r) - E(r)\}^2}{E(r)},}{\sum_{r=1}^{2^p} (O_r - E_r)^2 / E_r,} where \eqn{r}
  represents a response pattern, \eqn{O(r)}{O_r} and \eqn{E(r)}{E_r} represent the observed and expected frequencies, 
  respectively and \eqn{p} denotes the number of items. The Bootstrap approximation to the reference distribution is preferable compared with 
  the ordinary Chi-squared approximation since the latter is not valid especially for large number of items 
  (=> many response patterns with expected frequencies smaller than 1).
  
  In particular, the Bootstrap test is implemented as follows:
  \describe{
    \item{Step 0:}{Based on \code{object} compute the observed value of the statistic \eqn{T_{obs}}.}
    \item{Step 1:}{Simulate new parameter values, say \eqn{\theta^*}, from \eqn{N(\hat{\theta}, C(\hat{\theta}))},
    where \eqn{\hat{\theta}} are the MLEs and \eqn{C(\hat{\theta})} their large sample covariance 
    matrix.}
    \item{Step 2:}{Using \eqn{\theta^*} simulate new data (with the same dimensions as the 
    observed ones), fit the generalized partial credit or the Rasch model and based on this fit calculate the value 
    of the statistic \eqn{T_i}.}
    \item{Step 3:}{Repeat steps 1-2 \code{B} times and estimate the \eqn{p}-value using 
    \eqn{[1 + \sum\limits_{i=1}^B I(T_i  > T_{obs})] / (B + 1).}{[1 + \{\# T_i  > T_{obs}\}]/(B + 1).}}
  }
  
  Furthermore, in \code{GoF.gpcm} when \code{simulate.p.value = FALSE}, then the \eqn{p}-value is based on the asymptotic
  chi-squared distribution.
}

\value{
An object of class \code{GoF.gpcm} or \code{GoF.rasch} with components,
    \item{Tobs}{the value of the Pearson's chi-squared statistic for the observed data.}
    \item{B}{the \code{B} argument specifying the number of Bootstrap samples used.}
    \item{call}{the matched call of \code{object}.}
    \item{p.value}{the \eqn{p}-value of the test.}
    \item{simulate.p.value}{the value of \code{simulate.p.value} argument (returned on for class \code{GoF.gpcm}).}
    \item{df}{the degrees of freedom for the asymptotic chi-squared distribution (returned on for class \code{GoF.gpcm}).}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{person.fit}},
    \code{\link{item.fit}},
    \code{\link{margins}},
    \code{\link{gpcm}},
    \code{\link{rasch}}
}

\examples{

## GoF for the Rasch model for the LSAT data:
fit <- rasch(LSAT)
GoF.rasch(fit)

}

\keyword{multivariate}
