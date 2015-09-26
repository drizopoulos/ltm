\name{residuals}
\alias{residuals.gpcm}
\alias{residuals.grm}
\alias{residuals.ltm}
\alias{residuals.rasch}
\alias{residuals.tpm}

\title{ Residuals for IRT models }

\description{
  Computes the residuals for vectors of response patterns.
}

\usage{
\method{residuals}{gpcm}(object, resp.patterns = NULL, order = TRUE, \dots)

\method{residuals}{grm}(object, resp.patterns = NULL, order = TRUE, \dots)

\method{residuals}{ltm}(object, resp.patterns = NULL, order = TRUE, \dots)

\method{residuals}{rasch}(object, resp.patterns = NULL, order = TRUE, \dots)

\method{residuals}{tpm}(object, resp.patterns = NULL, order = TRUE, \dots)
}

\arguments{
  \item{object}{an object inheriting either from class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch} or class 
        \code{tpm}.}
  \item{resp.patterns}{a \code{matrix} or a \code{data.frame} of response patterns with columns denoting the 
        items; if \code{NULL} the expected frequencies are computed for the observed response patterns.}
  \item{order}{logical; if \code{TRUE} the response patterns are sorted according to the residual estimates.}
  \item{\dots}{additional arguments; currently none is used.}
}

\details{
The following residuals are computed: \deqn{\frac{O_i - E_i}{\sqrt{E_i}},}{(O_i - E_i) / E_i^(1/2),} where
\eqn{O_i} and \eqn{E_i} denote the observed and expected frequencies for the \eqn{i}th response pattern.
}

\value{A numeric \code{matrix} containing the observed and expected frequencies as well as the residual value for
each response pattern.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{fitted.gpcm}},
    \code{\link{fitted.grm}},
    \code{\link{fitted.ltm}},
    \code{\link{fitted.rasch}},
    \code{\link{fitted.tpm}}
}

\examples{

fit <- ltm(LSAT ~ z1)
residuals(fit)
residuals(fit, order = FALSE)

}

\keyword{methods}
