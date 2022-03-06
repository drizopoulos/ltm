\name{ltm}
\alias{ltm}
\title{ Latent Trait Model - Latent Variable Model for Binary Data}
\description{
  Fit a latent trait model under the Item Response Theory (IRT) approach.
}
\usage{
ltm(formula, constraint = NULL, IRT.param, start.val, 
    na.action = NULL, control = list())
}
\arguments{
  \item{formula}{ a two-sided formula providing the responses data matrix and describing the latent 
        structure. In the left side of \code{formula} either a \code{data.frame} (that will be converted to 
        a numeric matrix using \code{data.matrix()}) or a numeric \code{matrix} of manifest variables must be 
        supplied. In the right side of \code{formula} only 
        two latent variables are allowed with codenames \code{z1}, \code{z2}. Interaction and quadratic 
        terms can also be used (see \bold{Details} and \bold{Examples} for more info).}
  \item{constraint}{a three-column numeric matrix with at most \eqn{pq - 1} rows (where \eqn{p} is the number 
        of items and \eqn{q} the number of latent components plus the intercept), specifying fixed-value 
        constraints. The first column represents the item (i.e., \eqn{1} denotes the first item, \eqn{2} the 
        second, etc.), the second column represents the component of the latent structure (i.e., \eqn{1} 
        denotes the intercept \eqn{\beta_{0i}}{beta_{0i}}, \eqn{2} the loadings of the first factor \eqn{\beta_
        {1i}}{beta_{1i}}, etc.) and the third column denotes the value at which the corresponding parameter 
        should be fixed. See \bold{Details} and \bold{Examples} for more info.}
  \item{IRT.param}{logical; if \code{TRUE} then the coefficients' estimates for the two-parameter logistic 
        model are reported under the usual IRT parameterization. See \bold{Details} for more info.}
  \item{start.val}{the character string "random" or a numeric matrix supplying starting values with \eqn{p} rows and 
        \eqn{q} columns, with \eqn{p} denoting the number of items, and \code{q} denoting the number of terms in the 
        right-hand side of \code{formula}. If \code{NULL} starting values are automatically computed. If "random", 
        random starting values are used. If a matrix, then depending on the latent structure specified in \code{formula}, 
        the first column should contain \eqn{\beta_{0i}}, the second \eqn{\beta_{1i}}, the third \eqn{\beta_{2i}}, and 
        the remaing columns \eqn{\beta_{nl,i}} (see \bold{Details})}.
  \item{na.action}{ the \code{na.action} to be used on the data frame in the left side of \code{formula}.
        In case of missing data, if \code{na.action = NULL} the model uses the available cases, i.e., it takes 
        into account the observed part of sample units with missing values (valid under MAR mechanisms if the
        model is correctly specified). If you want to apply a complete case analysis then use 
        \code{na.action = na.exclude}.}
  \item{control}{ a list of control values,
    \describe{
        \item{iter.em}{ the number of EM iterations. Default 40.}
        \item{iter.qN}{ the number of quasi-Newton iterations. Default 150.}
        \item{GHk}{ the number of Gauss-Hermite quadrature points. Default 15.}
        \item{method}{ the optimization method to be used in \code{optim()}. Default "BFGS".}
        \item{verbose}{ logical; if \code{TRUE} info about the optimization procedure are printed.}
    }
  }
}

\details{
    The latent trait model is the analogue of the factor analysis model for binary observed data. 
    The model assumes that the dependencies between the observed response variables (known as items) 
    can be interpreted by a small number of latent variables. The model formulation is under the IRT 
    approach; in particular, \deqn{\log\left(\frac{\pi_{i}}{1-\pi_{i}}\right)=\beta_{0i} + \beta_{1i}z_1 + 
    \beta_{2i}z_2,}{logit(pi_i)=beta_{0i} + beta_{1i}z_1 + beta_{2i}z_2,} where \eqn{\pi_i} is the the 
    probability of a positive response in the \eqn{i}th item, \eqn{\beta_{i0}} is the easiness parameter, 
    \eqn{\beta_{ij}} (\eqn{j=1,2}) are the discrimination parameters and \eqn{z_1, z_2} denote the two 
    latent variables.

    The usual form of the latent trait model assumes linear latent variable effects (Bartholomew and 
    Knott, 1999; Moustaki and Knott, 2000). \code{ltm()} fits the linear one- and two-factor models but 
    also provides extensions described by Rizopoulos and Moustaki (2006) to include nonlinear latent 
    variable effects. These are incorporated in the linear predictor of the model, i.e., \deqn{\log\left
    (\frac{\pi_{i}}{1-\pi_{i}}\right)=\beta_{0i} + \beta_{1i}z_1 + \beta_{2i}z_2 + \beta_{nl}^tf(z_1, z_2),
    }{logit(pi_i)=beta_{0i} + beta_{1i}z_1 + beta_{2i}z_2 + beta_{nl}^tf(z_1, z_2),} where \eqn{f(z_1, z_2)} is
    a function of \eqn{z_1} and \eqn{z_2} (e.g., \eqn{f(z_1, z_2) = z_1z_2}, \eqn{f(z_1, z_2) = z_1^2}, etc.) and 
    \eqn{\beta_{nl}} is a matrix of nonlinear terms parameters (look also at the \bold{Examples}).
    
    If \code{IRT.param = TRUE}, then the parameters estimates for the two-parameter logistic
    model (i.e., the model with one factor) are reported under the usual IRT parameterization, i.e., 
    \deqn{\log\left(\frac{\pi_i}{1-\pi_i}\right) = \beta_{1i} (z - \beta_{0i}^*).}{logit(\pi_i) = 
    beta_{1i}(z - beta_{0i}^*).} 
    
    The linear two-factor model is unidentified under orthogonal rotations on the factors' 
    space. To achieve identifiability you can fix the value of one loading using the \code{constraint} 
    argument. 

    The parameters are estimated by maximizing the approximate marginal log-likelihood under the conditional 
    independence assumption, i.e., conditionally on the latent structure the items are independent Bernoulli 
    variates under the logit link. The required integrals are approximated using the Gauss-Hermite rule. The 
    optimization procedure used is a hybrid algorithm. The procedure initially uses a moderate number of EM 
    iterations (see \code{control} argument \code{iter.em}) and then switches to quasi-Newton (see \code{control} 
    arguments \code{method} and \code{iter.qN}) iterations until convergence.
}

\note{
    In the case of the one-factor model, the optimization algorithm works under the constraint that 
    the discrimination parameter of the first item \eqn{\beta_{11}}{beta_{11}} is always positive. If you wish 
    to change its sign, then in the fitted model, say \code{m}, use \code{m$coef[, 2] <- -m$coef[, 2]}.

    When the coefficients' estimates are reported under the usual IRT parameterization (i.e., \code{IRT.param = TRUE}),
    their standard errors are calculated using the Delta method.
}

\value{
An object of class \code{ltm} with components,
    \item{coefficients}{a matrix with the parameter values at convergence. These are always the estimates of 
        \eqn{\beta_{li}, l = 0, 1, \ldots}{beta_{li}, l = 0, 1, ...} parameters, even if \code{IRT.param = TRUE}.}
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
    \item{ltst}{a list describing the latent structure.}
    \item{X}{a copy of the response data matrix.}
    \item{control}{the values used in the \code{control} argument.}
    \item{IRT.param}{the value of the \code{IRT.param} argument.}
    \item{constraint}{\code{if(!is.null(constraint))}, then it contains the value of the \code{constraint} argument.}
    \item{call}{the matched call.}
}

\section{Warning}{ 
In case the Hessian matrix at convergence is not positive definite, try
to re-fit the model; \code{ltm()} will use new random starting values.

The inclusion of nonlinear latent variable effects produces more
complex likelihood surfaces which might possess a number of local
maxima. To ensure that the maximum likelihood value has been
reached re-fit the model a number of times (simulations showed
that usually 10 times are adequate to ensure global convergence).

Conversion of the parameter estimates to the usual IRT parameterization
works only for the two-parameter logistic model.
}

\references{
Baker, F. and Kim, S-H. (2004) \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Bartholomew, D. and Knott, M. (1999) \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.

Moustaki, I. and Knott, M. (2000) Generalized latent trait
models. \emph{Psychometrika}, \bold{65}, 391--411.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \doi{10.18637/jss.v017.i05}

Rizopoulos, D. and Moustaki, I. (2008) Generalized latent variable models
with nonlinear effects. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{61}, 415--438.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{coef.ltm}},
  \code{\link{fitted.ltm}},
  \code{\link{summary.ltm}},
  \code{\link{anova.ltm}},
  \code{\link{plot.ltm}},
  \code{\link{vcov.ltm}},
  \code{\link{item.fit}},
  \code{\link{person.fit}},  
  \code{\link{margins}},
  \code{\link{factor.scores}}
}

\examples{
## The two-parameter logistic model for the WIRS data
## with the constraint that (i) the easiness parameter 
## for the 1st item equals 1 and (ii) the discrimination
## parameter for the 6th item equals -0.5

ltm(WIRS ~ z1, constr = rbind(c(1, 1, 1), c(6, 2, -0.5)))


## One-factor and a quadratic term
## using the Mobility data
ltm(Mobility ~ z1 + I(z1^2))

## Two-factor model with an interaction term
## using the WIRS data
ltm(WIRS ~ z1 * z2)


## The two-parameter logistic model for the Abortion data 
## with 20 quadrature points and 20 EM iterations;
## report results under the usual IRT parameterization
ltm(Abortion ~ z1, control = list(GHk = 20, iter.em = 20))

}

\keyword{multivariate}
\keyword{regression}
