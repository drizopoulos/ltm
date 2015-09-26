\name{LSAT}
\alias{LSAT}
\docType{data}
\title{ The Law School Admission Test (LSAT), Section VI }
\description{
  The LSAT is a classical example in educational testing for measuring
  ability traits. This test was designed to measure a \emph{single}
  latent ability scale.
}
\format{
  A data frame with the responses of 1000 individuals to 5 questions.
}
\source{
  This LSAT example is a part of a data set given in Bock and Lieberman (1970).
}
\references{
Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.

Bock, R. and Lieberman, M. (1970) Fitting a response model for \eqn{n}
dichotomously scored items. \emph{Psychometrika}, \bold{35}, 179--197.

}
\examples{

## Descriptive statistics for LSAT data
dsc <- descript(LSAT)
dsc
plot(dsc)

}
\keyword{datasets}
