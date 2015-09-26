\name{testEquatingData}
\alias{testEquatingData}

\title{
    Prepares Data for Test Equating
}

\description{
  Test equating by common items.
}

\usage{
testEquatingData(DataList, AnchoringItems = NULL)
}

\arguments{
  \item{DataList}{ a list of \code{data.frame}s or \code{matrice}s containing common and unique items between 
        several forms.}
  \item{AnchoringItems}{ a \code{data.frame} or a \code{matrix} containing anchoring items for across sample equating.}
}

\details{
The purpose of this function is to combine items from different forms. Two cases are considered. Alternate 
Form Equating (where common and unique items are analyzed simultaneously) and Across Sample Equating (where different
sets of unique items are analyzed separately based on previously calibrated anchor items).
}

\value{
A \code{matrix} containing the common and unique items.
}

\references{
Yu, C.-H. and Osborn Popp, S. (2005) Test equating by common items and common subjects: concepts and applications.
\emph{Practical Assessment Research and Evaluation}, \bold{10(4)}, 1--19. URL 
\url{http://pareonline.net/getvn.asp?v=10&n=4}

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\examples{

# Let two data-sets with common and unique items
dat1 <- as.data.frame(rmvlogis(20, cbind(c(-2, 1, 2, 1), 1)))
names(dat1) <- c("CIt2", "CIt3", "CIt4", "W")

dat2 <- as.data.frame(rmvlogis(10, cbind(c(-2, -1, 1, 2, 0.95), 1)))
names(dat2) <- c("CIt1", "CIt2", "CIt3", "CIt4", "K")

# combine in one data-set by
lisForms <- list(dat1, dat2)
testEquatingData(lisForms)

}

\keyword{ multivariate }
