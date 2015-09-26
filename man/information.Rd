\name{information}
\alias{information}

\title{
    Area under the Test or Item Information Curves
}

\description{
  Computes the amount of test or item information for a fitted IRT model, in a specified range.
}

\usage{
information(object, range, items = NULL, ...)
}

\arguments{
  \item{object}{an object inheriting from either class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch} or class \code{tpm}.}
  \item{range}{a numeric interval for which the test information should be computed.}
  \item{items}{the items for which the information should be computed; the default \code{NULL} corresponds
                to all the items, which is equivalent to the test information.}
  \item{\dots}{extra arguments passed to \code{integrate()}.}
}

\details{
    The amount of information is computed as the area under the Item or Test Information Curve in the specified 
    interval, using \code{integrate()}.
}

\value{
A list of class \code{information} with components,
    \item{InfoRange}{the amount of information in the specified interval.}
    \item{InfoTotal}{the total amount of information; typically this is computed as the amount of information 
            in the interval \eqn{(-10, 10)}.}
    \item{PropRange}{the proportion of information in the specified range, i.e., 
        \code{"Info in range" / "Total Info"}.}
    \item{range}{the value of \code{range} argument.}
    \item{items}{the value of \code{items} argument.}
    \item{call}{the matched call for \code{object}.}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{plot.gpcm}},
  \code{\link{plot.grm}},
  \code{\link{plot.ltm}},
  \code{\link{plot.rasch}}
}

\examples{

fit <- rasch(LSAT)
information(fit, c(-2, 0))
information(fit, c(0, 2), items = c(3, 5))

}

\keyword{ multivariate }
