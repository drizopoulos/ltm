\name{item.fit}
\alias{item.fit}

\title{ Item-Fit Statistics and P-values }

\description{
  Computation of item fit statistics for \code{ltm}, \code{rasch} and \code{tpm} models.
}

\usage{
item.fit(object, G = 10, FUN = median, 
         simulate.p.value = FALSE, B = 100)
}

\arguments{
  \item{object}{a model object inheriting either from class \code{ltm}, class \code{rasch} or class \code{tpm}.}
  \item{G}{either a number or a numeric vector. If a number, then it denotes the number of categories sample units
        are grouped according to their ability estimates.}
  \item{FUN}{a function to summarize the ability estimate with each group (e.g., median, mean, etc.).}
  \item{simulate.p.value}{logical; if \code{TRUE}, then the Monte Carlo procedure described in the \bold{Details} 
        section is used to approximate the the distribution of the item-fit statistic under the null hypothesis.}  
  \item{B}{the number of replications in the Monte Carlo procedure.}
}

\details{
  The item-fit statistic computed by \code{item.fit()} has the form: \deqn{\sum \limits_{j = 1}^G \frac{N_j 
  (O_{ij} - E_{ij})^2}{E_{ij} (1 - E_{ij})},}{\sum_{j = 1}^g N_j (O_{ij} - E_{ij})^2 / [E_{ij} (1 - E_{ij})],}
  where \eqn{i} is the item, \eqn{j} is the interval created by grouping sample units on the basis of their ability
  estimates, \eqn{G} is the number of sample units groupings (i.e., \code{G} argument), \eqn{N_j} is the number of
  sample units with ability estimates falling in a given interval \eqn{j}, \eqn{O_{ij}} is the observed proportion of 
  keyed responses on item \eqn{i} for interval \eqn{j}, and \eqn{E_{ij}} is the expected proportion of keyed responses 
  on item \eqn{i} for interval \eqn{j} based on the IRT model (i.e., \code{object}) evaluated at the ability estimate 
  \eqn{z^*} within the interval, with \eqn{z^*} denoting the result of \code{FUN} applied to the ability estimates in 
  group \eqn{j}.
  
  If \code{simulate.p.value = FALSE}, then the \eqn{p}-values are computed assuming a chi-squared distribution with 
  degrees of freedom equal to the number of groups \code{G} minus the number of estimated parameters. If 
  \code{simulate.p.value = TRUE}, a Monte Carlo procedure is used to approximate the distribution of the item-fit 
  statistic under the null hypothesis. In particular, the following steps are replicated \code{B} times:
   \describe{
    \item{Step 1:}{Simulate a new data-set of dichotomous responses under the assumed IRT model, using the maximum
        likelihood estimates \eqn{\hat{\theta}}{\hat{theta}} in the original data-set, extracted from \code{object}.}
    \item{Step 2:}{Fit the model to the simulated data-set, extract the maximum likelihood estimates 
        \eqn{\theta^*}{theta^*} and compute the ability estimates \eqn{z^*} for each response pattern.}
    \item{Step 3:}{For the new data-set, and using \eqn{z^*} and \eqn{\theta^*}{theta^*}, compute the value of the 
        item-fit statistic.}
  }
  Denote by \eqn{T_{obs}} the value of the item-fit statistic for the original data-set. Then the \eqn{p}-value is 
  approximated according to the formula \deqn{\left(1 + \sum_{b = 1}^B I(T_b \geq T_{obs})\right) / (1 + B),}{(1 + 
  sum_{b = 1}^B I(T_b >= T_{obs})) / (1 + B),} where \eqn{I(.)} denotes the indicator function, and \eqn{T_b} denotes 
  the value of the item-fit statistic in the \eqn{b}th simulated data-set.
}

\value{
  An object of class \code{itemFit} is a list with components,
  \item{Tobs}{a numeric vector with item-fit statistics.}
  \item{p.values}{a numeric vector with the corresponding \eqn{p}-values.}
  \item{G}{the value of the \code{G} argument.}
  \item{simulate.p.value}{the value of the \code{simulate.p.value} argument.}
  \item{B}{the value of the \code{B} argument.}  
  \item{call}{a copy of the matched call of \code{object}.}
}

\references{

Reise, S. (1990) A comparison of item- and person-fit methods of assessing model-data fit in IRT. \emph{Applied
Psychological Measurement}, \bold{14}, 127--137.

Yen, W. (1981) Using simulation results to choose a latent trait model. \emph{Applied Psychological Measurement}, 
\bold{5}, 245--262.

}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{person.fit}},
    \code{\link{margins}},
    \code{\link{GoF.gpcm}},
    \code{\link{GoF.rasch}}
}

\examples{

# item-fit statistics for the Rasch model
# for the Abortion data-set
item.fit(rasch(Abortion))

# Yen's Q1 item-fit statistic (i.e., 10 latent ability groups; the
# mean ability in each group is used to compute fitted proportions) 
# for the two-parameter logistic model for the LSAT data-set
item.fit(ltm(LSAT ~ z1), FUN = mean)

}

\keyword{ multivariate }
