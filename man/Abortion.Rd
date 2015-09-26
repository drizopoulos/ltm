\name{Abortion}
\alias{Abortion}
\docType{data}
\title{ Attitude Towards Abortion }
\description{
    The data contain responses given by 410 individuals to four out of seven items
    concerning attitude to abortion. A small number of individual did not answer
    to some of the questions and this data set contains only the complete cases.
}
\format{
  379 individuals answered to the following questions after being asked if
  the law should allow abortion under the circumstances presented under each item,
  \describe{
    \item{Item 1}{The woman decides on her own that she does not.}
    \item{Item 2}{The couple agree that they do not wish to have a child.}
    \item{Item 3}{The woman is not married and does not wish to marry the man.}
    \item{Item 4}{The couple cannot afford any more children.}
  }
}
\source{
  1986 British Social Attitudes Survey (McGrath and Waterton, 1986).
}
\references{
  Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002) \emph{The Analysis and Interpretation of 
  Multivariate Data for Social Scientists}. London: Chapman and Hall.
  
  Knott, M., Albanese, M. and Galbraith, J. (1990) Scoring attitudes
  to abortion. \emph{The Statistician}, \bold{40}, 217--223.
  
  McGrath, K. and Waterton, J. (1986) \emph{British social attitudes, 1983-86 panel
  survey}. London: SCPR.
}
\examples{

## Descriptive statistics for Abortion data
dsc <- descript(Abortion)
dsc
plot(dsc)

}
\keyword{datasets}
