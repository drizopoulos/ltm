\name{rasch}
\alias{rasch}
\title{ Rasch Model }

\description{
  Fit the Rasch model under the Item Response Theory approach.
}

\usage{
rasch(data, constraint = NULL, IRT.param = TRUE, start.val = NULL, 
    na.action = NULL, control = list(), Hessian = TRUE)
}

\arguments{
  \item{data}{a \code{data.frame} (that will be converted to a numeric matrix using 
        \code{data.matrix()}) or a numeric \code{matrix} of manifest variables.}
  \item{constraint}{a two-column numeric matrix with at most \eqn{p} rows (where \eqn{p} is the number of items), 
        specifying fixed-value constraints. The first column represents the item (i.e., \eqn{1} denotes the first item,
        \eqn{2} the second, etc., and \eqn{p+1} the discrimination parameter) and the second column the 
        value at which the corresponding parameter should be fixed. See \bold{Examples} for more info.}
  \item{IRT.param}{logical; if \code{TRUE} then the coefficients' estimates are reported under the 
        usual IRT parameterization. See \bold{Details} for more info.}
  \item{start.val}{ the character string "random" or a numeric vector of \eqn{p+1} starting values, 
        where the first \eqn{p} values correspond to the easiness parameters while the last value corresponds to the 
        discrimination parameter. If "random", random starting values are used. If \code{NULL} starting values
        are automatically computed. }
  \item{na.action}{ the \code{na.action} to be used on \code{data}. In case of missing data, if 
        \code{na.action = NULL} the model uses the available cases, i.e., it takes into account the observed 
        part of sample units with missing values (valid under MAR mechanisms if the model is correctly specified). 
        If you want to apply a complete case analysis then use \code{na.action = na.exclude}.}
  \item{control}{a list of control values,
    \describe{
        \item{iter.qN}{ the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{ the number of Gauss-Hermite quadrature points. Default 21.}
        \item{method}{ the optimization method to be used in \code{optim()}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
    }
  }
  \item{Hessian}{logical; if \code{TRUE}, then the Hessian matrix is computed. Warning: setting this argument to \code{FALSE}
	will cause many methods (e.g., \code{summary()}) to fail; setting to \code{FALSE} is intended for simulation 
	purposes in order \code{rasch()} to run faster.}
}

\details{ 
    The Rasch model is a special case of the unidimensional latent trait model when all the discrimination 
    parameters are equal. This model was first discussed by Rasch (1960) and it is mainly used in educational 
    testing where the aim is to study the abilities of a particular set of individuals.

    The model is defined as follows \deqn{\log\left(\frac{\pi_i}{1-\pi_i}\right) = \beta_{i} + \beta z,}{ logit
    (\pi_i) = beta_{i} + beta z, } where \eqn{\pi_i} denotes the conditional probability of responding correctly 
    to the \eqn{i}th item given \eqn{z}, \eqn{\beta_{i}}{beta_{i}} is the easiness parameter for the \eqn{i}th 
    item, \eqn{\beta} is the discrimination parameter (the same for all the items) and \eqn{z} denotes the latent 
    ability.
    
    If \code{IRT.param = TRUE}, then the parameters estimates are reported under the usual IRT parameterization,
    i.e., \deqn{\log\left(\frac{\pi_i}{1-\pi_i}\right) = \beta (z - \beta_i^*).}{ logit
    (\pi_i) = beta (z - beta_i^*).}
    
    The fit of the model is based on approximate marginal Maximum Likelihood, using the Gauss-Hermite quadrature rule 
    for the approximation of the required integrals.
}

\note{
    Although the common formulation of the Rasch model assumes that the discrimination parameter is fixed to 1,
    \code{rasch()} estimates it. If you wish to fit the constrained version of the model, use the \code{constraint} 
    argument accordingly. See \bold{Examples} for more info.

    The optimization algorithm works under the constraint that the discrimination parameter 
    \eqn{\beta}{beta} is always positive.
    
    When the coefficients' estimates are reported under the usual IRT parameterization (i.e., \code{IRT.param = TRUE}),
    their standard errors are calculated using the Delta method.
}

\value{ An object of class \code{rasch} with components,
    \item{coefficients}{a matrix with the parameter values at convergence. These are always the estimates of 
        \eqn{\beta_i, \beta}{beta_i, beta} parameters, even if \code{IRT.param = TRUE}.}
    \item{log.Lik}{the log-likelihood value at convergence.}
    \item{convergence}{the convergence identifier returned by \code{optim()}.}
    \item{hessian}{the approximate Hessian matrix at convergence returned by \code{optim()}.}
    \item{counts}{the number of function and gradient evaluations used by the quasi-Newton algorithm.}
    \item{patterns}{a list with two components: (i) \code{X}: a numeric matrix 
        that contains the observed response patterns, and (ii) \code{obs}: a numeric vector that contains the observed 
        frequencies for each observed response pattern.}
    \item{GH}{a list with two components used in the Gauss-Hermite rule: (i) \code{Z}: a numeric matrix that contains 
        the abscissas, and (ii) \code{GHw}: a numeric vector that contains the corresponding  weights.} 
    \item{max.sc}{the maximum absolute value of the score vector at convergence.}
    \item{constraint}{the value of the \code{constraint} argument.}
    \item{IRT.param}{the value of the \code{IRT.param} argument.}
    \item{X}{a copy of the response data matrix.}
    \item{control}{the values used in the \code{control} argument.}
    \item{na.action}{the value of the \code{na.action} argument.}
    \item{call}{the matched call.}
}

\section{Warning}{ 
    In case the Hessian matrix at convergence is not positive definite, try to re-fit the model using 
    \code{rasch(..., start.val = "random")}.
}

\references{
Baker, F. and Kim, S-H. (2004) \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Rasch, G. (1960) \emph{Probabilistic Models for Some 
Intelligence and  Attainment Tests}. Copenhagen: Paedagogiske 
Institute.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{coef.rasch}},
  \code{\link{fitted.rasch}},
  \code{\link{summary.rasch}},
  \code{\link{anova.rasch}},
  \code{\link{plot.rasch}},
  \code{\link{vcov.rasch}},
  \code{\link{GoF.rasch}},
  \code{\link{item.fit}},
  \code{\link{person.fit}},  
  \code{\link{margins}},
  \code{\link{factor.scores}}
}

\examples{
## The common form of the Rasch model for the 
## LSAT data, assuming that the discrimination
## parameter equals 1
rasch(LSAT, constraint = cbind(ncol(LSAT) + 1, 1))


## The Rasch model for the LSAT data under the 
## normal ogive; to do that fix the discrimination
## parameter to 1.702
rasch(LSAT, constraint = cbind(ncol(LSAT) + 1, 1.702))

## The Rasch model for the LSAT data with
## unconstraint discrimination parameter
rasch(LSAT)

## The Rasch model with (artificially created) 
## missing data
data <- LSAT
data[] <- lapply(data, function(x){
    x[sample(1:length(x), sample(15, 1))] <- NA
    x
})
rasch(data)
}

\keyword{multivariate}
\keyword{regression}
