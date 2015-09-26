\name{vcov}
\alias{vcov.gpcm}
\alias{vcov.grm}
\alias{vcov.ltm}
\alias{vcov.rasch}
\alias{vcov.tpm}

\title{ vcov method for fitted IRT models }

\description{
 Extracts the asymptotic variance-covariance matrix of the MLEs from either \code{gpcm}, \code{grm},
 \code{ltm}, \code{rasch} or \code{tpm} objects.
}

\usage{

\method{vcov}{gpcm}(object, robust = FALSE, \dots)

\method{vcov}{grm}(object, \dots)

\method{vcov}{ltm}(object, robust = FALSE, \dots)

\method{vcov}{rasch}(object, robust = FALSE, \dots)

\method{vcov}{tpm}(object, \dots)
}

\arguments{
  \item{object}{an object inheriting from either class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch} or class 
    \code{tpm}.}
  \item{robust}{logical; if \code{TRUE} the sandwich estimator is used. }
  \item{\dots}{additional arguments; currently none is used. }
}

\value{
    a numeric matrix representing the estimated covariance matrix of the maximum likelihood estimates. Note that this 
    covariance matrix is for the parameter estimates under the additive parameterization and not under the usual IRT 
    parameterization; for more info check the \bold{Details} section of \code{\link{grm}}, \code{\link{ltm}}, 
    \code{\link{rasch}}, and \code{\link{tpm}}.
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
fit <- rasch(WIRS)
vcov(fit)
sqrt(diag(vcov(fit))) # standard errors under additive parameterization
}

\keyword{methods}
