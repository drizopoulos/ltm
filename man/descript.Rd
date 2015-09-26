\name{descript}
\alias{descript}

\title{
    Descriptive Statistics
}

\description{
  Computes descriptive statistics for dichotomous and polytomous response matrices.
}

\usage{
descript(data, n.print = 10, chi.squared = TRUE, B = 1000)
}

\arguments{
  \item{data}{ a \code{matrix} or a \code{data.frame} containing the manifest variables as columns. }
  \item{n.print}{numeric indicating the number of pairwise associations with the highest \eqn{p}-values 
        to be printed.}
  \item{chi.squared}{logical; if \code{TRUE} the chi-squared test for the pairwise associations between items
        is performed. See \bold{Details} for more info.}
  \item{B}{an integer specifying the number of replicates used in the Monte Carlo test (i.e., this is the \code{B} 
        argument of \code{chisq.test()}).}
}

\details{
    The following descriptive statistics are returned by \code{descript()}:
    \describe{
        \item{(i)}{the proportions for all the possible response categories for each item. In case all items are
                dichotomous, the logit of the proportion for the positive responses is also included.}
        \item{(ii)}{the frequencies of all possible total scores. The total score of a response pattern is simply its sum.
                For dichotomous items this is the number of positive responses, whereas for polytomous items this is 
                the sum of the levels represented as numeric values (e.g., the response categories "very concerned", 
                "slightly concerned", and "not very concerned" in \code{\link{Environment}} are represented as 1, 2, 
                and 3).}
        \item{(iii)}{Cronbach's alpha, for all items and excluding each time one of the items.}
        \item{(iv)}{for dichotomous response matrices two versions of the point biserial correlation of each item with the 
                total score are returned. In the first one the item is included in the computation of the
                total score, and in the second one is excluded.}
        \item{(v)}{pairwise associations between items. Before an analysis with latent variable models, it is useful to 
                inspect the data for evidence of positive correlations. In the case of binary or polytomous data, this 
                ad hoc check is performed by constructing the \eqn{2 \times 2}{2 by 2} contingency tables for all 
                possible pairs of items and examine the Chi-squared \eqn{p}-values. In case any expected frequencies 
                are smaller than 5, \code{simulate.p.value} is turned to \code{TRUE} in \code{chisq.test()}, 
                using \code{B} resamples.}
    }
}

\value{
\code{descript()} returns an object of class \code{descript} with components,
    \item{sample}{a numeric vector of length 2, with elements the number of items and the number of sample units.}
    \item{perc}{a numeric matrix containing the percentages of negative and positive responses for each item. If
        \code{data} contains only dichotomous manifest variables the logit of the positive responses (i.e.,
        second row) is also included.}
    \item{items}{a numeric matrix containing the frequencies for the total scores.}
    \item{pw.ass}{a matrix containing the \eqn{p}-values for the pairwise association between the items.}
    \item{n.print}{the value of the \code{n.print} argument.}
    \item{name}{the name of argument \code{data}.}
    \item{missin}{a numeric matrix containing the frequency and percentages of missing values for each item;
        returned only if any \code{NA}'s exist in \code{data}.}
    \item{bisCorr}{a numeric vector containing sample estimates of the biserial correlation of dichotomous manifest
        variables with the total score.}
    \item{ExBisCorr}{a numeric vector containing sample estimates of the biserial correlation of dichotomous manifest
        variables with the total score, where the latter is computed by excluding the specific item.}        
    \item{data}{a copy of the \code{data}.}
    \item{alpha}{a numeric matrix with one column containing the sample estimates of Cronbach's alpha, for all items
        and excluding each time one item.}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{plot.descript}},
    \code{\link{unidimTest}}
}

\examples{

## Descriptives for LSAT data:
dsc <- descript(LSAT, 3)
dsc
plot(dsc, type = "b", lty = 1, pch = 1:5)
legend("topleft", names(LSAT), pch = 1:5, col = 1:5, lty = 1, bty = "n")

}

\keyword{ multivariate }
