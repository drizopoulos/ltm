\name{anova}
\alias{anova.gpcm}
\alias{anova.grm}
\alias{anova.ltm}
\alias{anova.rasch}
\alias{anova.tpm}

\title{ Anova method for fitted IRT models}

\description{
  Performs a Likelihood Ratio Test between two nested IRT models. 
}

\usage{
\method{anova}{gpcm}(object, object2, simulate.p.value = FALSE, 
    B = 200, verbose = getOption("verbose"), seed = NULL, \dots)

\method{anova}{grm}(object, object2, \dots)

\method{anova}{ltm}(object, object2, \dots)

\method{anova}{rasch}(object, object2, \dots)

\method{anova}{tpm}(object, object2, \dots)
}
\arguments{
  \item{object}{an object inheriting from either class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch} 
                 or class \code{tpm}, representing the model under the null hypothesis.}
  \item{object2}{an object inheriting from either class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch},
                 or class \code{tpm}, representing the model under the alternative hypothesis.}
  \item{simulate.p.value}{logical; if \code{TRUE}, the reported \eqn{p}-value is based on a parametric Bootstrap approach.}
  \item{B}{the number of Bootstrap samples.}
  \item{verbose}{logical; if \code{TRUE}, information is printed in the console during the parametric Bootstrap.}
  \item{seed}{the seed to be used during the parametric Bootstrap; if \code{NULL}, a random seed is used.}
  \item{\dots}{ additional arguments; currently none is used.}
}

\details{
\code{anova.gpcm()} also includes the option to estimate the \eqn{p}-value of the LRT using a parametric Bootstrap approach. 
In particular, \code{B} data sets are simulated under the null hypothesis (i.e., under the generalized partial credit model 
\code{object}), and both the null and alternative models are fitted and the value of LRT is computed. Then the \eqn{p}-value is 
approximate using \eqn{[1 + \sum\limits_{i=1}^B I(T_i  > T_{obs})] / (B + 1),}{[1 + \{\# T_i  > T_{obs}\}]/(B + 1),} where \eqn{T_{obs}}
is the value of the likelihood ratio statistic in the original data set, and \eqn{T_i} the value of the statistic in the \eqn{i}th 
Bootstrap sample.

In addition, when \code{simulate.p.value = TRUE} objects of class \code{aov.gpcm} have a method for the \code{plot()} generic function
that produces a QQ plot comparing the Bootstrap sample of likelihood ration statistic with the asymptotic chi-squared distribution. For instance,
you can use something like the following: \code{lrt <- anova(obj1, obj2, simulate.p.value = TRUE); plot(lrt)}. 
}

\value{
An object of either class \code{aov.gpcm}, \code{aov.grm}, class \code{aov.ltm} or class \code{aov.rasch} with components,
    \item{nam0}{the name of \code{object}.}
    \item{L0}{the log-likelihood under the null hypothesis (\code{object}).}
    \item{nb0}{the number of parameter in \code{object}; returned only in \code{aov.gpcm}.}
    \item{aic0}{the AIC value for the model given by \code{object}.}
    \item{bic0}{the BIC value for the model given by \code{object}. }
    \item{nam1}{the name of \code{object2}.}
    \item{L1}{the log-likelihood under the alternative hypothesis (\code{object2}).}
    \item{nb1}{the number of parameter in \code{object}; returned only in \code{aov.gpcm}.}
    \item{aic1}{the AIC value for the model given by \code{object2}.}
    \item{bic1}{the BIC value for the model given by \code{object2}.}
    \item{LRT}{the value of the Likelihood Ratio Test statistic. }
    \item{df}{the degrees of freedom for the test (i.e., the difference in the number of parameters).}
    \item{p.value}{the \eqn{p}-value of the test.}
}

\section{Warning}{
    The code does not check if the models are nested! The user is responsible to supply nested models in
    order the LRT to be valid.
    
    When \code{object2} represents a three parameter model, note that the
    null hypothesis in on the boundary of the parameter space for the guessing parameters. Thus, the Chi-squared reference 
    distribution used by these function might not be totally appropriate.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{GoF.gpcm}},
    \code{\link{GoF.rasch}},
    \code{\link{gpcm}},
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}},
    \code{\link{tpm}}
}

\examples{

## LRT between the constrained and unconstrained GRMs 
## for the Science data:
fit0 <- grm(Science[c(1,3,4,7)], constrained = TRUE)
fit1 <- grm(Science[c(1,3,4,7)])
anova(fit0, fit1)


## LRT between the one- and two-factor models 
## for the WIRS data:
anova(ltm(WIRS ~ z1), ltm(WIRS ~ z1 + z2))


## An LRT between the Rasch and a constrained 
## two-parameter logistic model for the WIRS data: 
fit0 <- rasch(WIRS)
fit1 <- ltm(WIRS ~ z1, constraint = cbind(c(1, 3, 5), 2, 1))
anova(fit0, fit1)


## An LRT between the constrained (discrimination 
## parameter equals 1) and the unconstrained Rasch
## model for the LSAT data: 
fit0 <- rasch(LSAT, constraint = rbind(c(6, 1)))
fit1 <- rasch(LSAT)
anova(fit0, fit1)


## An LRT between the Rasch and the two-parameter 
## logistic model for the LSAT data: 
anova(rasch(LSAT), ltm(LSAT ~ z1))

}

\keyword{methods}
