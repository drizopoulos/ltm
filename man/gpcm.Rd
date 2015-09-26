\name{gpcm}
\alias{gpcm}

\title{Generalized Partial Credit Model - Polytomous IRT}

\description{
  Fits the Generalized Partial Credit model for ordinal polytomous data, under the Item Response Theory approach.
}

\usage{
gpcm(data, constraint = c("gpcm", "1PL", "rasch"), IRT.param = TRUE, 
    start.val = NULL, na.action = NULL, control = list())
}

\arguments{
  \item{data}{a \code{data.frame} or a numeric \code{matrix} of manifest variables.}
  \item{constraint}{a character string specifying which version of the Generalized Partial Credit Model to fit. See \bold{Details} and 
  \bold{Examples} for more info.}
  \item{IRT.param}{logical; if \code{TRUE} then the coefficients' estimates are reported under the 
        usual IRT parameterization. See \bold{Details} for more info.}
  \item{start.val}{a list of starting values or the character string \code{"random"}. If a list, each one of its 
        elements corresponds to each item and should contain a numeric vector with initial values for the 
        threshold parameters and discrimination parameter; even if \code{constraint = "rasch"} or \code{constraint = "1PL"}, 
        the discrimination parameter should be provided for all the items. If \code{"random"}, random starting values are computed.}
  \item{na.action}{the \code{na.action} to be used on \code{data}; default \code{NULL} the model uses the available 
        cases, i.e., it takes into account the observed part of sample units with missing values (valid under MAR 
        mechanisms if the model is correctly specified).}
  \item{control}{a named list of control values with components,
    \describe{
        \item{iter.qN}{the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{the number of Gauss-Hermite quadrature points. Default 21.}
        \item{optimizer}{which optimization routine to use; options are "optim" and "nlminb", the latter being the default.}
        \item{optimMethod}{the optimization method to be used in \code{optim()}. Default is "BFGS".}
        \item{numrDeriv}{which numerical derivative algorithm to use to approximate the Hessian matrix; options are "fd" for
                forward difference approximation and "cd" for central difference approximation. Default is "fd".}
        \item{epsHes}{step size to be used in the numerical derivative. Default is 1e-06. If you choose \code{numrDeriv = "cd"}, then
                change this to a larger value, e.g., 1e-03 or 1e-04.}
        \item{parscale}{the \code{parscale} control argument of \code{optim()}. Default is 0.5 for all parameters.}        
        \item{verbose}{logical; if \code{TRUE} info about the optimization procedure are printed.}
    }
  }
}

\details{ 
    The Generalized Partial Credit Model is an IRT model, that can handle ordinal manifest variables.
    This model was discussed by Masters (1982) and it was extended by Muraki (1992).

    The model is defined as follows \deqn{P_{ik}(z) = \frac{\exp \sum \limits_{c = 0}^k \beta_i (z - \beta_{ic}^*)}{
    \sum \limits_{r = 0}^{m_i} \exp \sum \limits_{c = 0}^r \beta_i (z - \beta_{ic}^*)},} where \eqn{P_{ik}(z)} denotes the 
    probability of responding in category \eqn{k} for item \eqn{i}, given the latent ability \eqn{z}, \eqn{\beta_{ic}^*} are the item-category 
    parameters, \eqn{\beta_i} is the discrimination parameter, \eqn{m_i} is the number of categories for item \eqn{i}, and 
    \deqn{\sum \limits_{c = 0}^0 \beta_i (z - \beta_{ic}^*) \equiv 0.} 

    If \code{constraint = "rasch"}, then the discrimination parameter \eqn{\beta_i} is assumed equal for all items and fixed at one. If 
    \code{constraint = "1PL"}, then the discrimination parameter \eqn{\beta_i} is assumed equal for all items but is estimated. 
    If \code{constraint = "gpcm"}, then each item has its one discrimination parameter \eqn{\beta_i} that is estimated. See 
    \bold{Examples} for more info. 
    
    If \code{IRT.param = FALSE}, then the linear predictor is of the form \eqn{\beta_i z + \beta_{ic}}.
    
    The fit of the model is based on approximate marginal Maximum Likelihood, using the Gauss-Hermite quadrature rule 
    for the approximation of the required integrals.
}

\note{
    \code{gpcm()} can also handle binary items and can be used instead of \code{\link{rasch}} and \code{\link{ltm}} though it is less
    efficient. However, \code{gpcm()} can handle a mix of dichotomous and polytomous items that neither \code{\link{rasch}} nor 
    \code{\link{ltm}} can.
}

\value{An object of class \code{gpcm} with components,
    \item{coefficients}{a named list with components the parameter values at convergence for each item.}
    \item{log.Lik}{the log-likelihood value at convergence.}
    \item{convergence}{the convergence identifier returned by \code{optim()} or \code{nlminb()}.}
    \item{hessian}{the approximate Hessian matrix at convergence.}
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
    In case the Hessian matrix at convergence is not positive definite try to re-fit the model by specifying the starting values or using
    \code{start.val = "random"}.
}

\references{
Masters, G. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika}, \bold{47}, 149--174.

Muraki, E. (1992). A generalized partial credit model: application of an EM algorithm. \emph{Applied Psychological 
Measurement}, \bold{16}, 159--176.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{coef.gpcm}},
  \code{\link{fitted.gpcm}},
  \code{\link{summary.gpcm}},
  \code{\link{anova.gpcm}},
  \code{\link{plot.gpcm}},
  \code{\link{vcov.gpcm}},
  \code{\link{GoF.gpcm}},
  \code{\link{margins}},
  \code{\link{factor.scores}}
}

\examples{

## The Generalized Partial Credit Model for the Science data:
gpcm(Science[c(1,3,4,7)])

## The Generalized Partial Credit Model for the Science data,
## assuming equal discrimination parameters across items:
gpcm(Science[c(1,3,4,7)], constraint = "1PL")

## The Generalized Partial Credit Model for the Science data,
## assuming equal discrimination parameters across items
## fixed at 1:
gpcm(Science[c(1,3,4,7)], constraint = "rasch")

## more examples can be found at:
## http://wiki.r-project.org/rwiki/doku.php?id=packages:cran:ltm#sample_analyses

}

\keyword{multivariate}
\keyword{regression}
