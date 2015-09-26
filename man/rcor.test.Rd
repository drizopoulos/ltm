\name{rcor.test}
\alias{rcor.test}

\title{
    Pairwise Associations between Items using a Correlation Coefficient
}

\description{
  Computes and tests the pairwise associations between items using a correlation coefficient
}

\usage{
rcor.test(mat, p.adjust = FALSE, p.adjust.method = "holm", \dots)
}

\arguments{
  \item{mat}{ a numeric \code{matrix} or a numeric \code{data.frame} containing the manifest variables as columns. }
  \item{p.adjust}{logical; if \code{TRUE} the \eqn{p}-values are adjusted for multiple comparisons.}
  \item{p.adjust.method}{the \code{method} argument of \code{p.adjust()}.}
  \item{\dots}{extra arguments passed to \code{cor()} and \code{cor.test()}.}
}

\value{
An object of class \code{rcor.test} with components,
    \item{cor.mat}{the correlation matrix.}
    \item{p.values}{a three column numeric matrix containing the \eqn{p}-values for all the combinations of items.}

The print method for class \code{rcor.test} returns a square matrix in which the upper diagonal part contains
the estimates of the correlation coefficients, and the lower diagonal part contains the corresponding \eqn{p}-values.
}

\note{
    \code{rcor.test()} is more appropriate for informal testing of association between polytomous items.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\examples{

## pairwise associations for Environment data:
rcor.test(data.matrix(Environment), method = "kendall")

## pairwise associations for independent normal random variates:
mat <- matrix(rnorm(1000), 100, 10, dimnames = list(NULL, LETTERS[1:10]))
rcor.test(mat)
rcor.test(mat, method = "kendall")
rcor.test(mat, method = "spearman")

}
\keyword{ multivariate }
