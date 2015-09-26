\name{plot descript}
\alias{plot.descript}

\title{Descriptive Statistics Plot method}

\description{
    The plot method for \code{descript} objects currently works for dichotomous response patterns, and produces the 
    xy-plot of the total score versus the proportion of correct responses for each item.
}

\usage{

\method{plot}{descript}(x, items = NULL, includeFirstLast = FALSE, xlab, ylab, \dots)

}

\arguments{
  \item{x}{an object inheriting from class \code{descript}.}
  \item{items}{a numeric vector indicating which items to plot.}
  \item{includeFirstLast}{logical; if \code{TRUE} the first and last total scores categories are included.}
  \item{xlab, ylab}{character string or an \code{\link{expression}}; see \code{\link{title}}.}
  \item{\dots}{ extra graphical parameters to be passed to \code{matplot()}.}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{descript}}
}

\examples{

## Descriptives for WIRS data:
dsc <- descript(WIRS)
dsc
plot(dsc, includeFirstLast = TRUE, type = "b", lty = 1, pch = 1:6)
legend("topleft", names(WIRS), pch = 1:6, col = 1:6, lty = 1, bty = "n")

}

\keyword{methods}
