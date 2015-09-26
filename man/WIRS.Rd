\name{WIRS}
\alias{WIRS}
\docType{data}
\title{Workplace Industrial Relation Survey Data}
\description{
 These data were taken from a section of the 1990 Workplace 
 Industrial Relation Survey (WIRS) dealing with management/worker 
 consultation in firms. The questions asked are given below:
}
\format{
  Please consider the most recent change involving the introduction 
  of the new plant, machinery and equipment. Were discussions or 
  consultations of any of the type on this card held either about the 
  introduction of the change or about the way it was to be implemented.
  \describe{
    \item{Item 1}{Informal discussion with individual workers.}
    \item{Item 2}{Meeting with groups of workers.}
    \item{Item 3}{Discussions in established joint consultative committee.}
    \item{Item 4}{Discussions in specially constituted committee to consider the change.}
    \item{Item 5}{Discussions with the union representatives at the establishment.}
    \item{Item 6}{Discussions with paid union officials from outside.}
  }
}
\source{
  The WIRS surveys can be found at \url{http://www.niesr.ac.uk/niesr/wers98/}.
}
\references{
  Bartholomew, D. (1998) Scaling unobservable constructs in social science. 
  \emph{Applied Statistics}, \bold{47}, 1--13. 
  
  Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002) \emph{The Analysis and Interpretation of 
  Multivariate Data for Social Scientists}. London: Chapman and Hall.
}
\examples{

## Descriptive statistics for Wirs data
descript(WIRS)

}
\keyword{datasets}
