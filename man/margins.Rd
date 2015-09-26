\name{margins}
\alias{margins}
\alias{margins.gpcm}
\alias{margins.grm}
\alias{margins.ltm}
\alias{margins.rasch}
\alias{margins.tpm}

\title{
    Fit of the model on the margins
}

\description{
  Checks the fit on the two- and three-way margins for \code{grm}, \code{ltm}, \code{rasch} and \code{tpm} objects.
}

\usage{

margins(object, \dots)

\method{margins}{gpcm}(object, type = c("two-way", "three-way"), rule = 3.5, \dots)

\method{margins}{grm}(object, type = c("two-way", "three-way"), rule = 3.5, \dots)

\method{margins}{ltm}(object, type = c("two-way", "three-way"), rule = 3.5, 
        nprint = 3, \dots)

\method{margins}{rasch}(object, type = c("two-way", "three-way"), rule = 3.5, 
        nprint = 3, \dots)

\method{margins}{tpm}(object, type = c("two-way", "three-way"), rule = 3.5, 
        nprint = 3, \dots)
}

\arguments{
  \item{object}{ an object inheriting either from class \code{gpcm}, class \code{grm}, class \code{ltm} or class \code{rasch}.}
  \item{type}{ the type of margins to be used. See \bold{Details} for more info. }
  \item{rule}{ the rule of thumb used in determining the indicative goodness-of-fit.}
  \item{nprint}{ a numeric value determining the number of margins with the largest Chi-squared residuals 
        to be printed; only for \code{ltm} and \code{rasch} objects.}
  \item{\dots}{ additional argument; currently none is used. }
}

\details{
    Rather than looking at the whole set of response patterns, we can look at the two- and three-way margins. 
    For the former, we construct the \eqn{2 \times 2}{2 by 2} contingency tables obtained by taking 
    the variables two at a time. Comparing the observed and expected two-way margins is analogous to comparing 
    the observed and expected correlations when judging the fit of a factor analysis model. For Bernoulli and
    Ordinal variates, the comparison is made using the so called Chi-squared residuals. As a rule of thumb residuals 
    greater than 3.5 are indicative of poor fit. For a more strict rule of thumb use the \code{rule} argument. 
    The analogous procedure is followed for the three-way margins.
}

\value{
An object of either class \code{margins.ltm} if \code{object} inherits from class \code{ltm}, class \code{rasch} or class \code{tpm}, 
            or an object of class \code{margins.grm} if \code{object} inherits from class \code{grm}, with components,
    \item{margins}{for \code{margins.ltm} is an array containing the values of chi-squared residuals; 
                    for \code{margins.gpcm} and \code{margins.grm} is a list of length either the number of all possible pairs or all possible 
                    triplets of items, containing the observed and expected frequencies, the values of chi-squared 
                    residuals, the value of the total residual and the value of the rule of thumb times the product of
                    the number of categories of the items under consideration.}
    \item{type}{the type of margins that were calculated.}
    \item{nprint}{the value of the \code{nprint} argument; returned only from \code{margins.ltm}.}
    \item{combs}{all possible two- or three-way combinations of the items; returned only from \code{margins.ltm}.}
    \item{rule}{the value of the \code{rule} argument; returned only from \code{margins.ltm}.}
    \item{nitems}{the number of items in \code{object}; returned only from \code{margins.grm}.}
    \item{names}{the names of items in \code{object}; returned only from \code{margins.grm}.}
    \item{call}{a copy of the matched call of \code{object}.}
}

\references{
Bartholomew, D. (1998) Scaling unobservable constructs in social science. 
\emph{Applied Statistics}, \bold{47}, 1--13.


Bartholomew, D. and Knott, M. (1999) \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}

}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{person.fit}},
    \code{\link{item.fit}},
    \code{\link{GoF.rasch}},
}

\examples{

## Two- and Three-way residuals for the Rasch model
fit <- rasch(LSAT)
margins(fit)
margins(fit, "three")


## Two- and Three-way residuals for the one-factor model
fit <- ltm(WIRS ~ z1)
margins(fit)
margins(fit, "three")


## Two- and Three-way residuals for the graded response model
fit <- grm(Science[c(1,3,4,7)])
margins(fit)
margins(fit, "three")

}

\keyword{methods}
