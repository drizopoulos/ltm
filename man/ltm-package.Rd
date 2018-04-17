\name{ltm-package}

\alias{ltm-package}

\docType{package}

\title{
Latent Trait Models for Item Response Theory Analyses
}

\description{
This package provides a flexible framework for Item Response Theory analyses for dichotomous and polytomous
data under a Marginal Maximum Likelihood approach. The fitting algorithms provide valid inferences under Missing At 
Random missing data mechanisms.
}

\details{
\tabular{ll}{
Package: \tab ltm\cr
Type: \tab Package\cr
Version: \tab 1.1-1\cr
Date: \tab 2018-04-17\cr
License: \tab \acronym{GPL} \cr
}
The following options are available:
\describe{
  \item{Descriptives:}{samples proportions, missing values information, biserial correlation of items with total score, 
        pairwise associations between items, Cronbach's \eqn{\alpha}{alpha}, unidimensionality check using modified 
        parallel analysis, nonparametric correlation coefficient, plotting of sample proportions versus total score.}
  \item{Dichotomous data:}{Rasch Model, Two Parameter Logistic Model, Birnbaum's Three Parameter Model, and 
        Latent Trait Model up to two latent variables (allowing also for nonlinear terms between the latent traits).}
  \item{Polytomous data:}{Samejima's Graded Response Model and the Generalized Partial Credit Model.}
  \item{Goodness-of-Fit:}{Bootstrapped Pearson \eqn{\chi^2} for Rasch and Generalized Partial Credit models, fit on the two- and three-way margins 
        for all models, likelihood ratio tests between nested models (including AIC and BIC criteria values),
        and item- and person-fit statistics.}
  \item{Factor Scoring - Ability Estimates:}{Empirical Bayes (i.e., posterior modes), Expected a posteriori (i.e., 
        posterior means), Multiple Imputed Empirical Bayes, and Component Scores for dichotomous data.}
  \item{Test Equating:}{Alternate Form Equating (where common and unique items are analyzed simultaneously) and Across 
        Sample Equating (where different sets of unique items are analyzed separately based on previously calibrated 
        anchor items).}
  \item{Plotting:}{Item Characteristic Curves, Item Information Curves, Test Information Functions, Standard Error 
        of Measurement, Standardized Loadings Scatterplot (for the two-factor latent trait model), Item Operation 
        Characteristic Curves (for ordinal polytomous data), Item Person Maps.}  
}
}

\author{
Dimitris Rizopoulos

Maintainer: Dimitris Rizopoulos <d.rizopoulos@erasmusmc.nl>
}

\references{
Baker, F. and Kim, S-H. (2004) \emph{Item Response Theory}, 2nd ed. 
New York: Marcel Dekker.

Rizopoulos, D. (2006) \bold{ltm}: An R package for latent variable modelling and item response theory analyses. 
\emph{Journal of Statistical Software}, \bold{17(5)}, 1--25. URL \url{http://www.jstatsoft.org/v17/i05/}
}

\keyword{ package }

\keyword{ multivariate }
