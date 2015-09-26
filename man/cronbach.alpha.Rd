\name{cronbach.alpha}

\alias{cronbach.alpha}

\title{

    Cronbach's alpha

}

\description{

  Computes Cronbach's alpha for a given data-set.

}

\usage{

cronbach.alpha(data, standardized = FALSE, CI = FALSE, 
    probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

}

\arguments{
  \item{data}{a \code{matrix} or a \code{data.frame} containing the items as columns.}
  \item{standardized}{logical; if \code{TRUE} the standardized Cronbach's alpha is computed.}
  \item{CI}{logical; if \code{TRUE} a Bootstrap confidence interval for Cronbach's alpha is computed.}
  \item{probs}{a numeric vector of length two indicating which quantiles to use for the Bootstrap CI.}
  \item{B}{the number of Bootstrap samples to use.}
  \item{na.rm}{logical; what to do with \code{NA}'s.}
}

\details{ 
  The Cronbach's alpha computed by \code{cronbach.alpha()} is defined as follows \deqn{\alpha = 
  \frac{p}{p - 1}\left(1 - \frac{\sum_{i=1}^p \sigma_{y_i}^2}{\sigma_x^2}\right),}{alpha = (p / (p - 1)) (1 - 
  (\sum_{i=1}^p sigma_{y_i}^2/ sigma_x^2)),} where \eqn{p} is the number of items \eqn{\sigma_x^2}{sigma_x^2} 
  is the variance of the observed total test scores, and \eqn{\sigma_{y_i}^2}{sigma_{y_i}^2} is the variance 
  of the \eqn{i}th item.
  
  The standardized Cronbach's alpha computed by \code{cronbach.alpha()} is defined as follows \deqn{\alpha_s = 
  \frac{p \cdot \bar{r}}{1 + (p - 1) \cdot \bar{r}},}{alpha_s = (p r) / (1 + (p - 1) r),} where \eqn{p} is the 
  number of items, and \eqn{\bar{r}}{r} is the average of all (Pearson) correlation coefficients between the 
  items. In this case if \code{na.rm = TRUE}, then the complete observations (i.e., rows) are used.
  
  The Bootstrap confidence interval is calculated by simply taking \code{B} samples with replacement from \code{data},
  calculating for each \eqn{\alpha}{alpha} or \eqn{\alpha_s}{alpha_s}, and computing the quantiles according to 
  \code{probs}.
}

\value{
\code{cronbach.alpha()} returns an object of class \code{cronbachAlpha} with components
    \item{alpha}{the value of Cronbach's alpha.}
    \item{n}{the number of sample units.}
    \item{p}{the number of items.}
    \item{standardized}{a copy of the \code{standardized} argument.}
    \item{name}{the name of argument \code{data}.}
    \item{ci}{the confidence interval for alpha; returned if \code{CI = TRUE}.}
    \item{probs}{a copy of the \code{probs} argument; returned if \code{CI = TRUE}.}
    \item{B}{a copy of the \code{B} argument; returned if \code{CI = TRUE}.}
}

\references{
Cronbach, L. J. (1951) Coefficient alpha and the internal structure of tests. 
\emph{Psychometrika}, \bold{16}, 297--334.
}

\author{
Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}


\examples{

# Cronbach's alpha for the LSAT data-set
# with a Bootstrap 95\% CI
cronbach.alpha(LSAT, CI = TRUE, B = 500)

}

\keyword{ multivariate }
