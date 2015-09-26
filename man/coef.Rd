\name{coef}
\alias{coef.gpcm}
\alias{coef.grm}
\alias{coef.ltm}
\alias{coef.rasch}
\alias{coef.tpm}
\title{ Extract Estimated Loadings }
\description{
  Extracts the estimated parameters from either \code{grm}, \code{ltm}, \code{rasch} or \code{tpm} objects.
}
\usage{
\method{coef}{gpcm}(object, \dots)

\method{coef}{grm}(object, \dots)

\method{coef}{ltm}(object, standardized = FALSE, prob = FALSE, order = FALSE, \dots)

\method{coef}{rasch}(object, prob = FALSE, order = FALSE, \dots)

\method{coef}{tpm}(object, prob = FALSE, order = FALSE, \dots)
}

\arguments{
  \item{object}{ an object inheriting from either class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch} or class \code{tpm}. }
  \item{standardized}{ logical; if \code{TRUE} the standardized loadings are also returned. See  \bold{Details} 
        for more info.}
  \item{prob}{ logical; if \code{TRUE} the probability of a positive response for the median individual
        (i.e., \eqn{Pr(x_i = 1 | z = 0)}, with \eqn{i = 1, \ldots, p}{i = 1, ..., p} denoting the items) 
        is also returned.}
  \item{order}{ logical; if \code{TRUE} the items are sorted according to the difficulty estimates. }
  \item{\dots}{ additional arguments; currently none is used. }
}

\details{
  The standardization of the factor loadings is useful in order to form a link to the 
  Underlying Variable approach. In particular, the standardized form of the factor loadings
  represents the correlation coefficient between the latent variables and the underlying continuous variables
  based on which the dichotomous outcomes arise (see Bartholomew and Knott, 1999, p.87-88 or Bartholomew 
  \emph{et al.}, 2002, p.191).
  
  The standardized factor loadings are computed only for the linear one- and two-factor models, fitted by \code{ltm()}.
}

\value{A list or a matrix of the estimated parameters for the fitted model.}

\references{
Bartholomew, D. and Knott, M. (1999) \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{gpcm}},
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}},
    \code{\link{tpm}}
}

\examples{

fit <- grm(Science[c(1,3,4,7)])
coef(fit)

fit <- ltm(LSAT ~ z1)
coef(fit, TRUE, TRUE)

m <- rasch(LSAT)
coef(fit, TRUE, TRUE)

}

\keyword{methods}
