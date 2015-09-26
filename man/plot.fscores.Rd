\name{plot fscores}
\alias{plot.fscores}

\title{Factor Scores - Ability Estimates Plot method}

\description{
    Plots a Kernel Density Estimation of the distribution of the factor scores (i.e., person parameters). Provides 
    also the option to include in the plot the item difficulty parameters (similar to the Item Person Maps).
}

\usage{

\method{plot}{fscores}(x, bw = "nrd0", adjust = 2, kernel = "gaussian", 
    include.items = FALSE, tol = 0.2, xlab = "Ability", ylab = "Density", 
    main = "Kernel Density Estimation for Ability Estimates", 
    pch = 16, cex = 1.5, \dots)
}

\arguments{
  \item{x}{an object inheriting from class \code{fscores}.}
  \item{bw, adjust, kernel}{arguments to \code{density()}.}
  \item{include.items}{logical; if \code{TRUE} the item difficulty parameters are included in the plot.}
  \item{tol}{the tolerance used to group the item difficulty parameters, i.e., when \code{include.items = TRUE}
        the values \code{round(betas / tol) * tol} are plotted, where \code{beta} is the numeric vector of
        item difficulty parameters.}
  \item{xlab, ylab, main}{character string or an \code{\link{expression}}; see \code{\link{title}}.}
  \item{pch, cex}{arguments to \code{stripchart()}; used when \code{include.items = TRUE}.}
  \item{\dots}{extra graphical parameters to be passed to \code{plot.density()}.}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{factor.scores}}
}

\examples{

## Factor Scores for LSAT data:
fsc <- factor.scores(rasch(LSAT))
plot(fsc, include.items = TRUE, main = "KDE for Person Parameters")
legend("left", "item parameters", pch = 16, cex = 1.5, bty = "n")

}

\keyword{methods}
