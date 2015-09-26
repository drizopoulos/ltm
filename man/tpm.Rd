\name{tpm}
\alias{tpm}

\title{ Birnbaum's Three Parameter Model }

\description{
  Fit Birnbaum's three parameter model under the Item Response Theory approach.
}
\usage{
tpm(data, type = c("latent.trait", "rasch"), constraint = NULL, 
    max.guessing = 1, IRT.param = TRUE, start.val = NULL, 
    na.action = NULL, control = list())
}
\arguments{
  \item{data}{a \code{data.frame} (that will be converted to a numeric matrix using 
        \code{data.matrix()}) or a numeric \code{matrix} of manifest variables.}
  \item{type}{a character string indicating the type of model to fit. Available options are `rasch' that
        assumes equal discrimination parameter among items, and `latent.trait' (default) that assumes a different 
        discrimination parameter per item.}
  \item{constraint}{a three-column numeric matrix specifying fixed-value constraints. The first column represents 
        the item (i.e., \eqn{1} denotes the first item, \eqn{2} the second, etc.); the second column denotes the type 
        of parameter to fix for the item specified in the first column (i.e., \eqn{1} denotes the guessing parameters,
        \eqn{2} the easiness parameters, and \eqn{3} the discrimination parameters); the third column specifies the value
        at which the corresponding parameter should be fixed. See \bold{Examples} for more info.}
  \item{max.guessing}{a scalar between 0 and 1 denoting the upper bound for the guessing parameters.}
  \item{IRT.param}{logical; if \code{TRUE} then the coefficients' estimates are reported under the 
        usual IRT parameterization. See \bold{Details} for more info.}
  \item{start.val}{the character string "random" or a numeric matrix supplying starting values with \eqn{p} rows and 
        3 columns, with \eqn{p} denoting the number of items. If \code{NULL} starting values are automatically computed. 
        If "random", random starting values are used. If a matrix, then the first column should contain the guessing 
        parameter, the second \eqn{\beta_{1i}}, and the third \eqn{\beta_{2i}} (see \bold{Details}). 
        If \code{type == "rasch"}, then the third should contain the same number \eqn{p} times.}
  \item{na.action}{ the \code{na.action} to be used on \code{data}. In case of missing data, if 
        \code{na.action = NULL} the model uses the available cases, i.e., it takes into account the observed 
        part of sample units with missing values (valid under MAR mechanisms if the model is correctly specified). 
        If you want to apply a complete case analysis then use \code{na.action = na.exclude}.}
  \item{control}{a list of control values with elements,
    \describe{
        \item{optimizer}{a character string denoting the optimizer to use, either \code{"optim"} (default) 
                         or \code{"nlminb"}.}
        \item{iter.qN}{ scalar denoting the number of iterations in the optimization procedure. For \code{optim()}
                        this is passed to the control argument `maxit', whereas for \code{nlminb()} this is passed
                        to both control arguments `iter.max' and `eval.max'. Default 1000.}
        \item{GHk}{ scalar denoting the number of Gauss-Hermite quadrature points. Default 21.}
        \item{method}{ a character string denoting the optimization method to be used in \code{optim()}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
        \item{eps.hessian}{the step-length to use in the central difference approximation that approximates the hessian.
                        Default is \code{1e-03}.}
        \item{parscale}{a scaling numeric vector of length equal to the parameters to be estimated (taking into account 
                        any constraints). This is passed to either to the `parscale' control argument of \code{optim()} 
                        or to the `scale' argument of \code{nlminb()}. Default is 0.5 for the guessing parameters and 1 
                        for the discrimination and easiness parameters.}
    }
  }
}

\details{ 
    Birnbaum's three parameter model is usually employed to handle the phenomenon of non-random guessing in the case
    of difficult items.  

    The model is defined as follows \deqn{\pi_i = c_i + (1 - c_i) \frac{\exp(\beta_{1i} + \beta_{2i} z)}{1 + 
    \exp(\beta_{1i} + \beta_{2i} z)},}{ \pi_i = c_i + (1 - c_i) / [1 + exp\{-(beta_{1i} + beta_{2i} z)\}], } where 
    \eqn{\pi_i} denotes the conditional probability of responding correctly to the \eqn{i}th item given \eqn{z}, 
    \eqn{c_i} denotes the guessing parameter, \eqn{\beta_{1i}}{beta_{1i}} is the easiness parameter, 
    \eqn{\beta_{2i}}{beta_{2i}} is the discrimination parameter, and \eqn{z} denotes the 
    latent ability. In case \code{type = "rasch"}, \eqn{\beta_{2i}}{beta_{2i}} is assumed equal for all items.
    
    If \code{IRT.param = TRUE}, then the parameters estimates are reported under the usual IRT parameterization,
    i.e., \deqn{\pi_i = c_i + (1 - c_i) \frac{\exp[\beta_{2i} (z - \beta_{1i}^*)]}{1 + 
    \exp[\beta_{2i} (z - \beta_{1i}^*)]}.}{ \pi_i = c_i + (1 - c_i) / [1 + exp\{-beta_{2i} (z - beta_{1i}^*)\}].}
    
    The fit of the model is based on approximate marginal Maximum Likelihood, using the Gauss-Hermite quadrature rule 
    for the approximation of the required integrals.
}

\value{ An object of class \code{tpm} with components,
    \item{coefficients}{a matrix with the parameter values at convergence. These are always the estimates of 
        \eqn{\beta_i, \beta}{beta_i, beta} parameters, even if \code{IRT.param = TRUE}.}
    \item{log.Lik}{the log-likelihood value at convergence.}
    \item{convergence}{the convergence identifier returned by \code{optim()}.}
    \item{hessian}{the approximate Hessian matrix at convergence obtained using a central difference approximation.}
    \item{counts}{the number of function and gradient evaluations used by the optimization algorithm.}
    \item{patterns}{a list with two components: (i) \code{X}: a numeric matrix 
        that contains the observed response patterns, and (ii) \code{obs}: a numeric vector that contains the observed 
        frequencies for each observed response pattern.}
    \item{GH}{a list with two components used in the Gauss-Hermite rule: (i) \code{Z}: a numeric matrix that contains 
        the abscissas, and (ii) \code{GHw}: a numeric vector that contains the corresponding  weights.} 
    \item{max.sc}{the maximum absolute value of the score vector at convergence.}
    \item{type}{the value of the \code{type} argument.}
    \item{constraint}{the value of the \code{constraint} argument.}
    \item{max.guessing}{the value of the \code{max.guessing} argument.}
    \item{IRT.param}{the value of the \code{IRT.param} argument.}
    \item{X}{a copy of the response data matrix.}
    \item{control}{the values used in the \code{control} argument.}
    \item{na.action}{the value of the \code{na.action} argument.}
    \item{call}{the matched call.}
}

\section{Warning}{ 
    The three parameter model is known to have numerical problems like non-convergence or convergence on the boundary,
    especially for the guessing parameters. These problems usually result in a zero estimate for some guessing 
    parameters and/or in a non positive definite Hessian matrix or in a high absolute value for the score vector 
    (returned by the \code{summary} method) at convergence. In case of estimates on the boundary, the \code{constraint} 
    argument can be used to set the guessing parameter(s) for the problematic item(s) to zero. In addition, 
    \code{tpm()} has a number of control parameters that can be tuned in order to obtain successful convergence; 
    the most important of these are the starting values, the parameter scaling vector and the optimizer.
}

\references{

Baker, F. and Kim, S-H. (2004) \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Birnbaum, A. (1968). Some latent trait models and their use in inferring an examinee's ability. In F. M. Lord and
M. R. Novick (Eds.), \emph{Statistical Theories of Mental Test Scores}, 397--479. Reading, MA: Addison-Wesley.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}

}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{coef.tpm}},
  \code{\link{fitted.tpm}},
  \code{\link{summary.tpm}},
  \code{\link{anova.tpm}},
  \code{\link{plot.tpm}},
  \code{\link{vcov.tpm}},
  \code{\link{item.fit}},
  \code{\link{person.fit}},  
  \code{\link{margins}},
  \code{\link{factor.scores}}
}

\examples{

# the three parameter model
tpm(LSAT)

# use 'nlminb' as optimizer
tpm(LSAT, control = list(optimizer = "nlminb"))


# the three parameter model with equal 
# discrimination parameter across items
# fix the guessing parameter for the third item to zero
tpm(LSAT, type = "rasch", constraint = cbind(3, 1, 0))


# the three parameter model for the Abortion data
fit <- tpm(Abortion)
fit

# the guessing parameter estimates for items 1, 3, and 4 seem to be on
# the boundary; update the fit by fixing them to zero
update(fit, constraint = cbind(c(1, 3, 4), 1, 0))

}
\keyword{multivariate}
\keyword{regression}
