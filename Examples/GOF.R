#######################
# Goodness - of - Fit #
#######################


# install the latest version of the `ltm' package
# (if you do not already have it)
install.packages("ltm", dependencies = TRUE)

# load `ltm' package
library(ltm)



#######################################
# Fit IRT models to the LSAT data-set #
#######################################

# Rasch model with discrimination parameter equal to 1
fitRasch.c <- rasch(LSAT, constraint = cbind(ncol(LSAT) + 1, 1))
fitRasch.c

# Rasch model with unconstrained discrimination parameter
fitRasch <- rasch(LSAT)
fitRasch

# Two-Parameter Logistic model
fit2PL <- ltm(LSAT ~ z1)
fit2PL

# Three-Parameter model with discrimination parameter equal to 1
fit3PL.rc <- tpm(LSAT, type = "rasch", constraint = cbind(1:5, 3, 1))
fit3PL.rc

# Three-Parameter model with equal discrimination parameter across items
fit3PL.r <- tpm(LSAT, type = "rasch")
fit3PL.r

# Three-Parameter model -- no constraint
fit3PL <- tpm(LSAT)
fit3PL



########################################
# Pearson Chi-squared for Rasch models #
########################################

GoF.rasch(fitRasch.c, B = 100)
GoF.rasch(fitRasch, B = 100)



#####################################
# Fit on the margins for all models #
#####################################

# Two-way margins
margins(fitRasch.c)
margins(fitRasch)
margins(fit2PL)
margins(fit3PL.rc)
margins(fit3PL.r)
margins(fit3PL)


# Three-way margins
margins(fitRasch.c, type = "three-way")
margins(fitRasch, type = "three-way")
margins(fit2PL, type = "three-way")
margins(fit3PL.rc, type = "three-way")
margins(fit3PL.r, type = "three-way")
margins(fit3PL, type = "three-way")



#######################
# Item-fit Statistics #
#######################

item.fit(fitRasch.c)
item.fit(fitRasch)
item.fit(fit2PL)
item.fit(fit3PL.rc)
item.fit(fit3PL.r)
item.fit(fit3PL)


# group individuals in 4 groups,
# use the mean as function to summarize
# abilities in each group
item.fit(fitRasch.c, G = 4, FUN = mean)
item.fit(fitRasch, G = 4, FUN = mean)
item.fit(fit2PL, G = 4, FUN = mean)
item.fit(fit3PL.rc, G = 4, FUN = mean)
item.fit(fit3PL.r, G = 4, FUN = mean)
item.fit(fit3PL, G = 4, FUN = mean)


# use Monte Carlo to approximate the distribution
# of the statistic under the null
item.fit(fitRasch.c, simulate.p.value = TRUE)
item.fit(fitRasch, simulate.p.value = TRUE)
item.fit(fit2PL, simulate.p.value = TRUE)
item.fit(fit3PL.rc, simulate.p.value = TRUE)
item.fit(fit3PL.r, simulate.p.value = TRUE)
item.fit(fit3PL, simulate.p.value = TRUE)



#########################
# Person-fit Statistics #
#########################

person.fit(fitRasch.c)
person.fit(fitRasch)
person.fit(fit2PL)
person.fit(fit3PL.rc)
person.fit(fit3PL.r)
person.fit(fit3PL)


# compute person-fit statistic p-values for
# the two.sided alternative
person.fit(fitRasch.c, alternative = "two.sided")
person.fit(fitRasch, alternative = "two.sided")
person.fit(fit2PL, alternative = "two.sided")
person.fit(fit3PL.rc, alternative = "two.sided")
person.fit(fit3PL.r, alternative = "two.sided")
person.fit(fit3PL, alternative = "two.sided")


# use Monte Carlo to approximate the distribution
# of the statistic under the null
person.fit(fitRasch.c, simulate.p.value = TRUE)
person.fit(fitRasch, simulate.p.value = TRUE)
person.fit(fit2PL, simulate.p.value = TRUE)
person.fit(fit3PL.rc, simulate.p.value = TRUE)
person.fit(fit3PL.r, simulate.p.value = TRUE)
person.fit(fit3PL, simulate.p.value = TRUE)


# focus on specific response patterns
person.fit(fitRasch.c, resp.patterns = rbind(c(0,0,1,0,NA), c(0,0,1,1,0), c(1,0,1,0,0)), 
            simulate.p.value = TRUE, B = 2000)
person.fit(fitRasch, resp.patterns = rbind(c(0,0,1,0,NA), c(0,0,1,1,0), c(1,0,1,0,0)), 
            simulate.p.value = TRUE, B = 2000)
person.fit(fit2PL, resp.patterns = rbind(c(0,0,1,0,NA), c(0,0,1,1,0), c(1,0,1,0,0)), 
            simulate.p.value = TRUE, B = 2000)
person.fit(fit3PL.rc, resp.patterns = rbind(c(0,0,1,0,NA), c(0,0,1,1,0), c(1,0,1,0,0)), 
            simulate.p.value = TRUE, B = 2000)
person.fit(fit3PL.r, resp.patterns = rbind(c(0,0,1,0,NA), c(0,0,1,1,0), c(1,0,1,0,0)), 
            simulate.p.value = TRUE, B = 2000)
person.fit(fit3PL, resp.patterns = rbind(c(0,0,1,0,NA), c(0,0,1,1,0), c(1,0,1,0,0)), 
            simulate.p.value = TRUE, B = 2000)
