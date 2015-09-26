\name{factor.scores}
\alias{factor.scores}
\alias{factor.scores.gpcm}
\alias{factor.scores.grm}
\alias{factor.scores.ltm}
\alias{factor.scores.rasch}
\alias{factor.scores.tpm}

\title{ Factor Scores - Ability Estimates }

\description{
  Computation of factor scores for \code{grm}, \code{ltm}, \code{rasch} and \code{tpm} models.
}

\usage{
factor.scores(object, \dots)

\method{factor.scores}{gpcm}(object, resp.patterns = NULL, 
        method = c("EB", "EAP", "MI"), B = 5, robust.se = FALSE, 
        prior = TRUE, return.MIvalues = FALSE, \dots)

\method{factor.scores}{grm}(object, resp.patterns = NULL, 
        method = c("EB", "EAP", "MI"), B = 5, prior = TRUE, 
        return.MIvalues = FALSE, \dots)

\method{factor.scores}{ltm}(object, resp.patterns = NULL, 
        method = c("EB", "EAP", "MI", "Component"), B = 5, 
        robust.se = FALSE, prior = TRUE, return.MIvalues = FALSE, 
        \dots)

\method{factor.scores}{rasch}(object, resp.patterns = NULL, 
        method = c("EB", "EAP", "MI"), B = 5, robust.se = FALSE,
	    prior = TRUE, return.MIvalues = FALSE, \dots)

\method{factor.scores}{tpm}(object, resp.patterns = NULL, 
        method = c("EB", "EAP", "MI"), B = 5, prior = TRUE, 
        return.MIvalues = FALSE, \dots)
}

\arguments{
  \item{object}{an object inheriting from either class \code{gpcm}, class \code{grm}, class \code{ltm}, class \code{rasch} or class 
        \code{tpm}.}
  \item{resp.patterns}{a matrix or a data.frame of response patterns with columns denoting the items; if \code{NULL} 
        the factor scores are computed for the observed response patterns.}
  \item{method}{a character supplying the scoring method; available methods are:
        Empirical Bayes, Expected a Posteriori, Multiple Imputation, and Component. See \bold{Details} section for 
        more info.}
  \item{B}{the number of multiple imputations to be used if \code{method = "MI"}.}
  \item{robust.se}{logical; if \code{TRUE} the sandwich estimator is used for the estimation of the covariance
  matrix of the MLEs. See \bold{Details} section for more info.}
  \item{prior}{logical. If \code{TRUE}, then the prior normal distribution for the latent abilities is taken into
	account in the calculation of the posterior modes, when \code{method = "EB"}.}
  \item{return.MIvalues}{logical. If \code{TRUE}, then the estimated z-values and their covariance matrix are contained
    as extra attributes \code{"zvalues.MI"} and \code{"var.zvalues.MI"}, respectively, in the returned 
    \code{score.dat} data frame.}
  \item{\dots}{ additional arguments; currently none is used. }
}

\details{
  Factor scores or ability estimates are summary measures of the posterior distribution \eqn{p(z|x)}, 
  where \eqn{z} denotes the vector of latent variables and \eqn{x} the vector of manifest variables.
  
  Usually as factor scores we assign the modes of the above posterior distribution evaluated at the MLEs. These
  Empirical Bayes estimates (use \code{method = "EB"}) and their associated variance are good measures of the 
  posterior distribution while \eqn{p \rightarrow \infty}{p -> infinity}, where \eqn{p} is the number of items. 
  This is based on the result \deqn{p(z|x)=p(z|x; \hat{\theta})(1+O(1/p)),}{p(z|x)=p(z|x; \hat{\theta})(1+O(1/p)),}
  where \eqn{\hat{\theta}} are the MLEs. However, in cases where \eqn{p} and/or \eqn{n} (the sample size) is small 
  we ignore the variability of plugging-in estimates but not the \emph{true} parameter values. A solution to this 
  problem can be given using Multiple Imputation (MI; use \code{method = "MI"}). In particular, MI is used the 
  other way around, i.e.,
   \describe{
    \item{Step 1:}{Simulate new parameter values, say \eqn{\theta^*}, from \eqn{N(\hat{\theta}, C(\hat{\theta}))},
    where \eqn{C(\hat{\theta})} is the large sample covariance matrix of \eqn{\hat{\theta}} (if \code{robust.se = TRUE}, 
    \eqn{C(\hat{\theta})} is based on the sandwich estimator).}
    \item{Step 2:}{Maximize \eqn{p(z|x; \theta^*)} \emph{wrt} \eqn{z} and also compute the associated 
    variance to this mode.}
    \item{Step 3:}{Repeat steps 1-2 \code{B} times and combine the estimates using the known formulas of MI.}
  }
  
  This scheme explicitly acknowledges the ignorance of the true parameter values by drawing from their large sample
  posterior distribution while taking into account the sampling error. The modes of the posterior distribution 
  \eqn{p(z|x; \theta)}{p(z|x; theta)} are numerically approximated using the BFGS algorithm in \code{optim()}. 
  
  The Expected a posteriori scores (use \code{method = "EAP"}) computed by \code{factor.scores()} are defined as 
  follows: \deqn{\int z p(z | x; \hat{\theta}) dz.}
  
  The Component scores (use \code{method = "Component"}) proposed by Bartholomew (1984) is an alternative method
  to scale the sample units in the latent dimensions identified by the model that avoids the calculation of the 
  posterior mode. However, this method is not valid in the general case where nonlinear latent terms are assumed.
}

\value{
  An object of class \code{fscores} is a list with components,
  \item{score.dat}{the \code{data.frame} of observed response patterns including, observed and expected 
        frequencies (only if the observed data response matrix contains no missing vales), the factor scores 
        and their standard errors.}
  \item{method}{a character giving the scoring method used.}
  \item{B}{the number of multiple imputations used; relevant only if \code{method = "MI"}.}
  \item{call}{a copy of the matched call of \code{object}.}
  \item{resp.pats}{logical; is \code{TRUE} if \code{resp.patterns} argument has been specified.}
  \item{coef}{the parameter estimates returned by \code{coef(object)}; this is \code{NULL} when \code{object}
        inherits from class \code{grm}.}
}

\references{
Bartholomew, D. (1984) Scaling binary data using a factor model. \emph{Journal of the Royal 
Statistical Society, Series B}, \bold{46}, 120--123.

Bartholomew, D. and Knott, M. (1999) \emph{Latent Variable Models
and Factor Analysis}, 2nd ed. London: Arnold.

Bartholomew, D., Steel, F., Moustaki, I. and Galbraith, J. (2002)
\emph{The Analysis and Interpretation of Multivariate Data for
Social Scientists}. London: Chapman and Hall.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}

Rizopoulos, D. and Moustaki, I. (2008) Generalized latent variable models
with nonlinear effects. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{61}, 415--438.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{plot.fscores}},
    \code{\link{gpcm}},
    \code{\link{grm}},
    \code{\link{ltm}},
    \code{\link{rasch}},
    \code{\link{tpm}}
}

\examples{

## Factor Scores for the Rasch model
fit <- rasch(LSAT)
factor.scores(fit) # Empirical Bayes


## Factor scores for all subjects in the
## original dataset LSAT
factor.scores(fit, resp.patterns = LSAT)


## Factor scores for specific patterns,
## including NA's, can be obtained by 
factor.scores(fit, resp.patterns = rbind(c(1,0,1,0,1), c(NA,1,0,NA,1)))


\dontrun{
## Factor Scores for the two-parameter logistic model
fit <- ltm(Abortion ~ z1)
factor.scores(fit, method = "MI", B = 20) # Multiple Imputation

## Factor Scores for the graded response model
fit <- grm(Science[c(1,3,4,7)])
factor.scores(fit, resp.patterns = rbind(1:4, c(NA,1,2,3)))
}
}

\keyword{methods}
