\name{rmvlogis}
\alias{rmvlogis}
\alias{rmvordlogis}

\title{
    Generate Random Responses Patterns under Dichotomous and Polytomous IRT models
}

\description{
  Produces Bernoulli or Multinomial random variates under the Rasch, the two-parameter logistic, the three parameter, 
  the graded response, and the generalized partial credit models.
}

\usage{
rmvlogis(n, thetas, IRT = TRUE, link = c("logit", "probit"), 
         distr = c("normal", "logistic", "log-normal", "uniform"), 
         z.vals = NULL)

rmvordlogis(n, thetas, IRT = TRUE, model = c("gpcm", "grm"), 
    link = c("logit", "probit"), 
    distr = c("normal", "logistic", "log-normal", "uniform"), 
    z.vals = NULL)
}

\arguments{
  \item{n}{a scalar indicating the number of response patterns to simulate.}
  \item{thetas}{for \code{rmvlogis()} a numeric matrix with rows representing the items and columns the parameters.
    For \code{rmvordlogis()} a list with numeric vector elements, with first the threshold parameters and last the discrimination
    parameter. See \bold{Details} for more info.}
  \item{IRT}{ logical; if \code{TRUE} \code{thetas} are under the IRT parameterization. 
    See \bold{Details} for more info.}
  \item{model}{from which model to simulate.}
  \item{link}{ a character string indicating the link function to use. Options are logit and probit. }
  \item{distr}{ a character string indicating the distribution of the latent variable. Options are Normal, Logistic, 
    log-Normal, and Uniform. }
  \item{z.vals}{ a numeric vector of length \code{n} providing the values of the latent variable (ability) to be used 
    in the simulation of the dichotomous responses; if specified the value of \code{distr} is ignored.}
}

\details{
The binary variates can be simulated under the following parameterizations for the probability of correctly responding in 
the \eqn{i}th item. If \code{IRT = TRUE} \deqn{\pi_i = c_i + (1 - c_i) g(\beta_{2i} (z - \beta_{1i})),}{ \pi_i = c_i + 
(1 - c_i) g(beta_{2i} (z - beta_{1i})),} whereas if \code{IRT = FALSE}  \deqn{\pi_i = c_i + (1 - c_i) g(\beta_{1i} + 
\beta_{2i} z),}{ \pi_i = c_i + (1 - c_i) g(beta_{1i} + beta_{2i} z),} \eqn{z} denotes the latent variable, 
\eqn{\beta_{1i}} and \eqn{\beta_{2i}} are the first and second columns of \code{thetas}, respectively, and \eqn{g()} 
is the link function. If \code{thetas} is a three-column matrix then the third column should contain the guessing 
parameters \eqn{c_i}'s.

The ordinal variates are simulated according to the generalized partial credit model or the graded response model depending
on the value of the \code{model} argument. Check \code{\link{gpcm}} and \code{\link{grm}} to see how these models are defined,
under both parameterizations.
}

\note{
For options \code{distr = "logistic"}, \code{distr = "log-normal"} and \code{distr = "uniform"} the simulated random 
variates for \eqn{z} simulated under the Logistic distribution with \code{location = 0} and \code{scale = 1}, the 
log-Normal distribution with \code{meanlog = 0} and \code{sdlog = 1} and the Uniform distribution with \code{min = -3.5}
and \code{max = 3.5}, respectively. Then, the simulated \eqn{z} variates are standardized, using the theoretical mean 
and variance of the Logistic, log-Normal and Uniform distribution, respectively.
}

\value{
a numeric matrix with \code{n} rows and columns the number of items, containing the simulated binary or ordinal variates.
}

\author{
Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
  \code{\link{gpcm}},
  \code{\link{grm}},
  \code{\link{ltm}},
  \code{\link{rasch}},
  \code{\link{tpm}}
}

\examples{

# 10 response patterns under a Rasch model
# with 5 items
rmvlogis(10, cbind(seq(-2, 2, 1), 1))

# 10 response patterns under a GPCM model
# with 5 items, with 3 categories each
thetas <- lapply(1:5, function(u) c(seq(-1, 1, len = 2), 1.2))
rmvordlogis(10, thetas)

}

\keyword{ multivariate }
