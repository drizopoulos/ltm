\name{grm}
\alias{grm}
\title{ Graded Response Model - Polytomous IRT}
\description{
  Fits the Graded Response model for ordinal polytomous data, under the Item Response Theory approach.
}
\usage{
grm(data, constrained = FALSE, IRT.param = TRUE, Hessian = FALSE, 
    start.val = NULL, na.action = NULL, control = list())
}
\arguments{
  \item{data}{a \code{data.frame} (that will be converted to a numeric matrix using 
        \code{data.matrix()}) or a numeric \code{matrix} of manifest variables.}
  \item{constrained}{logical; if \code{TRUE} the model with equal discrimination parameters across items is fitted. 
        See \bold{Examples} for more info.}
  \item{IRT.param}{logical; if \code{TRUE} then the coefficients' estimates are reported under the 
        usual IRT parameterization. See \bold{Details} for more info.}
  \item{Hessian}{logical; if \code{TRUE} the Hessian matrix is computed.}
  \item{start.val}{ a list of starting values or the character string \code{"random"}. If a list, each one of its 
        elements corresponds to each item and should contain a numeric vector with initial values for the 
        extremity parameters and discrimination parameter; even if \code{constrained = TRUE} the discrimination 
        parameter should be provided for all the items. If \code{"random"} random starting values are computed.}
  \item{na.action}{ the \code{na.action} to be used on \code{data}; default \code{NULL} the model uses the available 
        cases, i.e., it takes into account the observed part of sample units with missing values (valid under MAR 
        mechanisms if the model is correctly specified)..}
  \item{control}{a list of control values,
    \describe{
        \item{iter.qN}{ the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{ the number of Gauss-Hermite quadrature points. Default 21.}
        \item{method}{ the optimization method to be used in \code{optim()}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
        \item{digits.abbrv}{ numeric value indicating the number of digits used in abbreviating the Item's names.
            Default 6.}
    }
  }
}

\details{ 
    The Graded Response Model is a type of polytomous IRT model, specifically designed for ordinal manifest variables.
    This model was first discussed by Samejima (1969) and it is mainly used in cases where the assumption of ordinal 
    levels of response options is plausible.

    The model is defined as follows \deqn{\log\left(\frac{\gamma_{ik}}{1-\gamma_{ik}}\right) = \beta_i z - 
    \beta_{ik},}{ logit(\gamma_ik) = beta_i z - beta_ik, } where \eqn{\gamma_{ik}} denotes the cumulative 
    probability of a response in category \eqn{k}th or lower to the \eqn{i}th item, given the latent ability \eqn{z}. 
    If \code{constrained = TRUE} it is assumed that \eqn{\beta_i = \beta} for all \eqn{i}.
    
    If \code{IRT.param = TRUE}, then the parameters estimates are reported under the usual IRT parameterization,
    i.e., \deqn{\log\left(\frac{\gamma_{ik}}{1-\gamma_{ik}}\right) = \beta_i (z - \beta_{ik}^*),}{ logit
    (\gamma_ik) = beta_i (z - beta_ik^*),} where \eqn{\beta_{ik}^* = \beta_{ik} / \beta_i}{beta_ik^* = beta_ik / beta_i}.

    The fit of the model is based on approximate marginal Maximum Likelihood, using the Gauss-Hermite quadrature rule 
    for the approximation of the required integrals.
}

\note{
    \code{grm()} returns the parameter estimates such that the discrimination parameter for the first item
    \eqn{\beta_1}{beta_1} is positive.
        
    When the coefficients' estimates are reported under the usual IRT parameterization (i.e., \code{IRT.param = TRUE}),
    their standard errors are calculated using the Delta method.
    
    \code{grm()} can also handle binary items, which should be coded as `1, 2' instead of `0, 1'.
    
    Some parts of the code used for the calculation of the log-likelihood and the score vector have been based 
    on \code{polr()} from package MASS.
}

\value{ An object of class \code{grm} with components,
    \item{coefficients}{a named list with components the parameter values at convergence for each item. These are always 
        the estimates of \eqn{\beta_{ik}, \beta_i}{beta_ik, beta_i} parameters, even if \code{IRT.param = TRUE}.}
    \item{log.Lik}{the log-likelihood value at convergence.}
    \item{convergence}{the convergence identifier returned by \code{optim()}.}
    \item{hessian}{the approximate Hessian matrix at convergence returned by \code{optim()}; returned 
            only if \code{Hessian = TRUE}.}
    \item{counts}{the number of function and gradient evaluations used by the quasi-Newton algorithm.}
    \item{patterns}{a list with two components: (i) \code{X}: a numeric matrix 
        that contains the observed response patterns, and (ii) \code{obs}: a numeric vector that contains the observed 
        frequencies for each observed response pattern.}
    \item{GH}{a list with two components used in the Gauss-Hermite rule: (i) \code{Z}: a numeric matrix that contains 
        the abscissas, and (ii) \code{GHw}: a numeric vector that contains the corresponding  weights.} 
    \item{max.sc}{the maximum absolute value of the score vector at convergence.}
    \item{constrained}{the value of the \code{constrained} argument.}
    \item{IRT.param}{the value of the \code{IRT.param} argument.}
    \item{X}{a copy of the response data matrix.}
    \item{control}{the values used in the \code{control} argument.}
    \item{na.action}{the value of the \code{na.action} argument.}
    \item{call}{the matched call.}
}

\section{Warning}{ 
    In case the Hessian matrix at convergence is not positive definite try to re-fit the model,
    using \code{start.val = "random"}.
}

\references{
Baker, F. and Kim, S-H. (2004) \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. 
\emph{Psychometrika Monograph Supplement}, \bold{34}, 100--114.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}

}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{coef.grm}},
  \code{\link{fitted.grm}},
  \code{\link{summary.grm}},
  \code{\link{anova.grm}},
  \code{\link{plot.grm}},
  \code{\link{vcov.grm}},
  \code{\link{margins}},
  \code{\link{factor.scores}}
}

\examples{

## The Graded Response model for the Science data:
grm(Science[c(1,3,4,7)])

## The Graded Response model for the Science data,
## assuming equal discrimination parameters across items:
grm(Science[c(1,3,4,7)], constrained = TRUE)

## The Graded Response model for the Environment data
grm(Environment)

}
\keyword{multivariate}
\keyword{regression}
