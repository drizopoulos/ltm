\name{summary}
\alias{summary.gpcm}
\alias{summary.grm}
\alias{summary.ltm}
\alias{summary.rasch}
\alias{summary.tpm}

\title{ Summary method for fitted IRT models }

\description{
  Summarizes the fit of either \code{grm}, \code{ltm}, \code{rasch} or \code{tpm} objects.
}

\usage{
\method{summary}{gpcm}(object, robust.se = FALSE, \dots)

\method{summary}{grm}(object, \dots)

\method{summary}{ltm}(object, robust.se = FALSE, \dots)

\method{summary}{rasch}(object, robust.se = FALSE, \dots)

\method{summary}{tpm}(object, \dots)
}

\arguments{
  \item{object}{ an object inheriting from either class \code{gpcm}, either class \code{grm}, class \code{ltm}, 
        class \code{rasch} or class \code{tpm}.}
  \item{robust.se}{logical; if \code{TRUE} robust estimation of standard errors is used, based on the sandwich estimator.}
  \item{\dots}{ additional argument; currently none is used. }
}

\note{
    For the parameters that have been constrained, the standard errors and \eqn{z}-values are printed as \code{NA}.

    When the coefficients' estimates are reported under the usual IRT parameterization (i.e., \code{IRT.param = TRUE}
    in the call of either \code{grm}, \code{ltm} or \code{rasch}), their standard errors are calculated using the 
    Delta method.
}

\value{
  An object of either class \code{summ.gpcm}, class \code{summ.grm}, class \code{summ.ltm} or class \code{summ.rasch} with components,
  \item{coefficients}{the estimated coefficients' table.}
  \item{Var.betas}{the approximate covariance matrix for the estimated parameters; returned only in \code{summ.ltm} 
            and \code{summ.rasch}.}
  \item{logLik}{the log-likelihood of \code{object}.}
  \item{AIC}{the AIC for \code{object}.}
  \item{BIC}{the BIC for \code{object}.}
  \item{max.sc}{the maximum absolute value of the score vector at convergence.}
  \item{conv}{the convergence identifier returned by \code{optim()}.}
  \item{counts}{the \code{counts} argument returned by \code{optim()}.}
  \item{call}{the matched call of \code{object}.}
  \item{ltn.struct}{a character vector describing the latent structure used in \code{object}; returned only in 
            \code{summ.ltm}.}
  \item{control}{the values used in the \code{control} argument in the fit of \code{object}.}
  \item{nitems}{the number of items in the data set; returned only in \code{summ.ltm} and \code{summ.rasch}.}
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

# use Hessian = TRUE if you want standard errors
fit <- grm(Science[c(1,3,4,7)], Hessian = TRUE)
summary(fit)

## One factor model using the WIRS data;
## results are reported under the IRT
## parameterization
fit <- ltm(WIRS ~ z1)
summary(fit)

}
\keyword{methods}
