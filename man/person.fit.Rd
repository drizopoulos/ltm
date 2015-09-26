\name{person.fit}
\alias{person.fit}

\title{ Person-Fit Statistics and P-values }

\description{
  Computation of person fit statistics for \code{ltm}, \code{rasch} and \code{tpm} models.
}

\usage{
person.fit(object, alternative = c("less", "greater", "two.sided"), 
           resp.patterns = NULL, FUN = NULL, simulate.p.value = FALSE, 
           B = 1000)
}

\arguments{
  \item{object}{a model object inheriting either from class \code{ltm}, class \code{rasch} or class \code{tpm}.}
  \item{alternative}{the alternative hypothesis; see \bold{Details} for more info.}
  \item{resp.patterns}{a matrix or a data.frame of response patterns with columns denoting the items; if \code{NULL} 
        the person fit statistics are computed for the observed response patterns.}
  \item{FUN}{a function with three arguments calculating a user-defined person-fit statistic. The first argument must
        be a numeric matrix of (0, 1) response patterns. The second argument must be a numeric vector of length equal to 
        the number of rows of the first argument, providing the ability estimates for each response pattern. The third
        argument must be a numeric matrix with number of rows equal to the number of items, providing the IRT model
        parameters. For \code{ltm} and \code{rasch} objects, this should be a two-column matrix, where the first
        column contains the easiness and the second one the discrimination parameters (i.e., the additive 
        parameterization is assumed, which has the form \eqn{\beta_{i0} + \beta_{i1}z}{beta_{i0} + beta_{i1}z}, where 
        \eqn{\beta_{i0}}{beta_{i0}} is the easiness and \eqn{\beta_{i1}}{beta_{i1}} the discrimination parameter for 
        the \eqn{i}th item). For \code{tpm} objects the first column of the third argument of \code{FUN} should contain 
        the logit (i.e., use \code{qlogis()}) of the guessing parameters, the second column the easiness, and the third
        column the discrimination parameters. The function should return a numeric vector of length equal to the number
        of response patterns, containing the values of the user-defined person-fit statistics.}
  \item{simulate.p.value}{logical; if \code{TRUE}, then the Monte Carlo procedure described in the \bold{Details} 
        section is used to approximate the the distribution of the person-fit statistic(s) under the null hypothesis.}
  \item{B}{the number of replications in the Monte Carlo procedure.}
}

\details{
  The statistics calculated by default (i.e., if \code{FUN = NULL}) by \code{person.fit()} are the \eqn{L_0} statistic 
  of Levine and Rubin (1979) and its standardized version \eqn{L_z} proposed by Drasgow et al. (1985). 
  If \code{simulate.p.value = FALSE}, the \eqn{p}-values are calculated for the \eqn{L_z} assuming a standard normal 
  distribution for the statistic under the null. If \code{simulate.p.value = TRUE}, a Monte Carlo procedure is used to 
  approximate the distribution of the person-fit statistic(s) under the null hypothesis. In particular, the following 
  steps are replicated \code{B} times for each response pattern:
   \describe{
    \item{Step 1:}{Simulate a new ability estimate, say \eqn{z^*}, from a normal distribution with mean the ability 
        estimate of the response pattern under the fitted model (i.e., \code{object}), and standard 
        deviation the standard error of the ability estimate, as returned by the \code{\link{factor.scores}} function.}
    \item{Step 2:}{Simulate a new response pattern of dichotomous items under the assumed IRT model, using \eqn{z^*} and 
       the maximum likelihood estimates under \code{object}.}
    \item{Step 4:}{For the new response pattern and using \eqn{z^*} and the MLEs, compute the values 
        of the person-fit statistic.}
  }
  Denote by \eqn{T_{obs}} the value of the person-fit statistic for the original data-set. Then the \eqn{p}-value is 
  approximated according to the formula \deqn{\left(1 + \sum_{b = 1}^B I(T_b \leq T_{obs})\right) / (1 + B),}{(1 + 
  sum_{b = 1}^B I(T_b <= T_{obs})) / (1 + B),} if \code{alternative = "less"}, \deqn{\left(1 + \sum_{b = 1}^B I(T_b \geq 
  T_{obs})\right) / (1 + B),}{(1 + sum_{b = 1}^B I(T_b >= T_{obs})) / (1 + B),} if \code{alternative = "greater"}, or 
  \deqn{\left(1 + \sum_{b = 1}^B I(|T_b| \geq |T_{obs}|)\right) / (1 + B),}{(1 + sum_{b = 1}^B I(|T_b| >= |T_{obs}|)) / 
  (1 + B),} if \code{alternative = "two.sided"}, where \eqn{T_b} denotes the value of the person-fit statistic in the 
  \eqn{b}th simulated data-set, \eqn{I(.)} denotes the indicator function, and \eqn{|.|} denotes the absolute value. 
  For the \eqn{L_z} statistic, negative values (i.e., \code{alternative = "less"}) indicate response patterns that 
  are unlikely, given the measurement model and the ability estimate. Positive values (i.e., \code{alternative = 
  "greater"}) indicate that the examinee's response pattern is more consistent than the probabilistic IRT model 
  expected. Finally, when \code{alternative = "two.sided"} both the above settings are captured.
  
  This simulation scheme explicitly accounts for the fact that ability values are estimated, by drawing 
  from their large sample distribution. Strictly speaking, drawing \eqn{z^*} from a normal distribution is not 
  theoretically appropriate, since the posterior distribution for the latent abilities is not normal. However, the 
  normality assumption will work reasonably well, especially when a large number of items is considered.
}

\value{
  An object of class \code{persFit} is a list with components,
  \item{resp.patterns}{the response patterns for which the fit statistics have been computed.}
  \item{Tobs}{a numeric matrix with person-fit statistics for each response pattern.}
  \item{p.values}{a numeric matrix with the corresponding \eqn{p}-values.}
  \item{statistic}{the value of the \code{statistic} argument.}
  \item{FUN}{the value of the \code{FUN} argument.}
  \item{alternative}{the value of the \code{alternative} argument.}
  \item{B}{the value of the \code{B} argument.}
  \item{call}{a copy of the matched call of \code{object}.}
}

\references{

Drasgow, F., Levine, M. and Williams, E. (1985) Appropriateness measurement with polychotomous item
response models and standardized indices. \emph{British Journal of Mathematical and Statistical Psychology}, 
\bold{38}, 67--86.

Levine, M. and Rubin, D. (1979) Measuring the appropriateness of multiple-choice test scores. \emph{Journal of
Educational Statistics}, \bold{4}, 269--290.

Meijer, R. and Sijtsma, K. (2001) Methodology review: Evaluating person fit. \emph{Applied
Psychological Measurement}, \bold{25}, 107--135.

Reise, S. (1990) A comparison of item- and person-fit methods of assessing model-data fit in IRT. \emph{Applied
Psychological Measurement}, \bold{14}, 127--137.
}

\author{
    Dimitris Rizopoulos \email{d.rizopoulos@erasmusmc.nl}
}

\seealso{
    \code{\link{item.fit}},
    \code{\link{margins}},
    \code{\link{GoF.gpcm}},
    \code{\link{GoF.rasch}}
}

\examples{

# person-fit statistics for the Rasch model
# for the Abortion data-set
person.fit(rasch(Abortion))

# person-fit statistics for the two-parameter logistic model
# for the LSAT data-set
person.fit(ltm(LSAT ~ z1), simulate.p.value = TRUE, B = 100)

}

\keyword{ multivariate }
