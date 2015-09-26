\name{unidimTest}
\alias{unidimTest}

\title{ Unidimensionality Check using Modified Parallel Analysis }

\description{
  An empirical check for the unidimensionality assumption for \code{ltm}, \code{rasch} and \code{tpm} models.
}

\usage{
unidimTest(object, data, thetas, IRT = TRUE, z.vals = NULL, 
           B = 100, \dots)
}

\arguments{
  \item{object}{a model object inheriting either from class \code{ltm}, class \code{rasch} or class \code{tpm}. For
        \code{ltm()} it is assumed that the two-parameter logistic model has been fitted (i.e., one latent variable and
        no nonlinear terms); see \bold{Note} for an extra option.}
  \item{data}{a \code{matrix} or a \code{data.frame} of response patterns with columns denoting the items; used 
        if \code{object} is missing.}
  \item{thetas}{a numeric \code{matrix} with IRT model parameter values to be used in \code{\link{rmvlogis}}; used if 
        \code{object} is missing.}
  \item{IRT}{logical, if \code{TRUE}, then argument \code{thetas} contains the measurement model parameters under the 
        usual IRT parameterization (see \code{\link{rmvlogis}}); used if \code{object} is missing.}
  \item{z.vals}{a numeric vector of length equal to the number of rows of \code{data}, providing ability estimates.
        If \code{object} is supplied then the abilities are estimated using \code{\link{factor.scores}}. If \code{NULL}, 
        the abilities are simulated from a standard normal distribution.}
  \item{B}{the number of samples for the Monte Carlo procedure to approximate the distribution of the statistic under 
        the null hypothesis.}
  \item{\dots}{extra arguments to \code{polycor()}.}
}

\details{
  This function implements the procedure proposed by Drasgow and Lissak (1983) for examining the latent dimensionality
  of dichotomously scored item responses. The statistic used for testing unidimensionality is the second eigenvalue of
  the tetrachoric correlations matrix of the dichotomous items. The tetrachoric correlations between are computed 
  using function \code{polycor()} from package `polycor', and the largest one is taken as communality estimate.
  
  A Monte Carlo procedure is used to approximate the distribution of this statistic under the null hypothesis. 
  In particular, the following steps are replicated \code{B} times:
   \describe{
    \item{Step 1:}{If \code{object} is supplied, then simulate new ability estimates, say \eqn{z^*}, from a normal 
        distribution with mean the ability estimates \eqn{\hat{z}} in the original data-set, and standard deviation 
        the standard error of \eqn{\hat{z}} (in this case the \code{z.vals} argument is ignored). If \code{object} 
        is not supplied and the \code{z.vals} argument has been specified, then set \eqn{z^* =} \code{z.vals}. Finally, 
        if \code{object} is not supplied and the \code{z.vals} argument has not been specified, then simulate \eqn{z^*} 
        from a standard normal distribution.}
    \item{Step 2:}{Simulate a new data-set of dichotomous responses, using \eqn{z^*}, and parameters the estimated 
        parameters extracted from \code{object} (if it is supplied) or the parameters given in the \code{thetas} 
        argument.}
    \item{Step 3:}{For the new data-set simulated in Step 2, compute the tetrachoric correlations matrix and take the
        largest correlations as communalities. For this matrix compute the eigenvalues.}
  }
  Denote by \eqn{T_{obs}} the value of the statistic (i.e., the second eigenvalue) for the original data-set. Then the 
  \eqn{p}-value is approximated according to the formula \eqn{\left(1 + \sum_{b = 1}^B I(T_b \geq T_{obs})\right) / 
  (1 + B)}{(1 + sum_{b = 1}^B I(T_b >= T_{obs})) / (1 + B)}, where \eqn{I(.)} denotes the indicator function, and 
  \eqn{T_b} denotes the value of the statistic in the \eqn{b}th data-set.
}

\note{
  For \code{ltm} objects you can also use a likelihood ratio test to check unidimensionality. In particular, 
  \code{fit0 <- ltm(data ~ z1); fit1 <- ltm(data ~ z1 + z2); anova(fit0, fit1)}.
}

\value{
  An object of class \code{unidimTest} is a list with components,
  \item{Tobs}{a numeric vector of the eigenvalues for the observed data-set.}
  \item{Tboot}{a numeric matrix of the eigenvalues for each simulated data-set.}
  \item{p.value}{the \eqn{p}-value.}
  \item{call}{a copy of the matched call of \code{object} if that was supplied.}
}

\references{
Drasgow, F. and Lissak, R. (1983) Modified parallel analysis: a procedure for examining the latent dimensionality
of dichotomously scored item responses. \emph{Journal of Applied Psychology}, \bold{68}, 363--373.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{descript}}
}

\examples{
\dontrun{
# Unidimensionality Check for the LSAT data-set
# under a Rasch model:
out <- unidimTest(rasch(LSAT))
out
plot(out, type = "b", pch = 1:2)
legend("topright", c("Real Data", "Average Simulated Data"), lty = 1, 
    pch = 1:2, col = 1:2, bty = "n")
}
}

\keyword{ multivariate }
