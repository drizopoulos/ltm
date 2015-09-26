\name{fitted}
\alias{fitted.gpcm}
\alias{fitted.grm}
\alias{fitted.ltm}
\alias{fitted.rasch}
\alias{fitted.tpm}

\title{ Fitted Values for IRT model}

\description{
  Computes the expected frequencies for vectors of response patterns.
}

\usage{
\method{fitted}{gpcm}(object, resp.patterns = NULL, 
    type = c("expected", "marginal-probabilities",
    "conditional-probabilities"), \dots)

\method{fitted}{grm}(object, resp.patterns = NULL, 
    type = c("expected", "marginal-probabilities",
    "conditional-probabilities"), \dots)

\method{fitted}{ltm}(object, resp.patterns = NULL, 
    type = c("expected", "marginal-probabilities", 
    "conditional-probabilities"), \dots)

\method{fitted}{rasch}(object, resp.patterns = NULL, 
    type = c("expected", "marginal-probabilities", 
    "conditional-probabilities"), \dots)

\method{fitted}{tpm}(object, resp.patterns = NULL, 
    type = c("expected", "marginal-probabilities", 
    "conditional-probabilities"), \dots)
}

\arguments{
  \item{object}{ an object inheriting either from class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch}, or 
        class \code{tpm}. }
  \item{resp.patterns}{a \code{matrix} or a \code{data.frame} of response patterns with columns denoting the 
        items; if \code{NULL} the expected frequencies are computed for the observed response patterns.}
  \item{type}{if \code{type == "marginal-probabilities"} the marginal probabilities for each response are
        computed; these are given by \eqn{\int \{ \prod_{i = 1}^p Pr(x_i = 1 | z)^{x_i} \times
        (1 - Pr(x_i = 1 | z))^{1 - x_i} \}p(z) dz}, where \eqn{x_i} denotes
        the \eqn{i}th item and \eqn{z} the latent variable. If \code{type == "expected"} the expected frequencies
        for each response are computed, which are the marginal probabilities times the number of sample units. If
        \code{type == "conditional-probabilities"} the conditional probabilities for each response and item are
        computed; these are \eqn{Pr(x_i = 1 | \hat{z})}, where \eqn{\hat{z}} is the ability estimate .}
  \item{\dots}{ additional arguments; currently none is used. }
}

\value{
a numeric \code{matrix} or a \code{list} containing either the response patterns of interest with their expected 
frequencies or marginal probabilities, if \code{type == "expected" || "marginal-probabilities"} or the conditional 
probabilities for each response pattern and item, if \code{type == "conditional-probabilities"}.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{residuals.gpcm}},
    \code{\link{residuals.grm}},
    \code{\link{residuals.ltm}},
    \code{\link{residuals.rasch}},
    \code{\link{residuals.tpm}}
}

\examples{
fit <- grm(Science[c(1,3,4,7)])
fitted(fit, resp.patterns = matrix(1:4, nr = 4, nc = 4))

fit <- rasch(LSAT)
fitted(fit, type = "conditional-probabilities")
}

\keyword{methods}
