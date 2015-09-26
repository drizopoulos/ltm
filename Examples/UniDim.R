############################################################
# Unidimensionality Check using Modified Parallel Analysis #
############################################################


# install the latest version of the `ltm' package
# (if you do not already have it)
install.packages("ltm", dependencies = TRUE)

# load `ltm' package
library(ltm)


# we illustrate the use of the unidimTest() function
# for the checking the unidimensionality for dichotomous
# items, asumming as parameters the maximum likelihood 
# estimates under the two-parameter logistic model; check
# ?unidimTest() for more info.
#
# We use 200 Monte Carlo samples, thus the following
# require some time to execute depending on your machine



#####################
# Abortion data-set #
#####################

out1 <- unidimTest(ltm(Abortion ~ z1), B = 200)
out1
par(mfrow = c(2, 2)) # if you wish to create a single plot for the three data-sets
plot(out1, type = "b", pch = 1:2, main = "Abortion data-set")
legend("topright", c("Real Data", "Average over Simulated Data"), lty = 1, 
    pch = 1:2, col = 1:2, bty = "n")


#################
# WIRS data-set #
#################

out2 <- unidimTest(ltm(WIRS ~ z1), B = 200)
out2
plot(out2, type = "b", pch = 1:2, main = "WIRS data-set")
legend("topright", c("Real Data", "Average over Simulated Data"), lty = 1, 
    pch = 1:2, col = 1:2, bty = "n")


#################
# LSAT data-set #
#################

out3 <- unidimTest(ltm(LSAT ~ z1), B = 200)
out3
plot(out3, type = "b", pch = 1:2, main = "LSAT data-set")
legend("topright", c("Real Data", "Average over Simulated Data"), lty = 1, 
    pch = 1:2, col = 1:2, bty = "n")



################################################################
# Note: with ltm() it is possible to check unidimensionality   #
# using a likelihood ratio test, assuming two latent variables #
# for the alternative, e.g.,                                   #
################################################################

fit0 <- ltm(Abortion ~ z1)
fit1 <- ltm(Abortion ~ z1 + z2)
anova(fit0, fit1)


fit0 <- ltm(WIRS ~ z1)
fit1 <- ltm(WIRS ~ z1 + z2)
anova(fit0, fit1)


fit0 <- ltm(LSAT ~ z1)
fit1 <- ltm(LSAT ~ z1 + z2)
anova(fit0, fit1)
