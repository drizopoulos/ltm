################################################################################################
# Sample analysis for the Sexual Attitude data-set.                                            #
# Description: The data set is extracted from the 1990 British Social Attitudes Survey.        #
# It concerns contemporary sexual attitudes. The questions addressed to 1077 individuals were  #
# as follows:                                                                                  #
# 1. Should divorce be easier?                                                                 #
# 2. Do you support the law against sexual discrimination?                                     #
# 3. View on pre-marital sex: (wrong/not wrong)                                                #
# 4. View on extra-marital sex: (wrong/not wrong)                                              #
# 5. View on sexual relationship between individuals of the same sex: (wrong/not wrong)        #
# 6. Should gays teach in school?                                                              #
# 7. Should gays teach in higher education?                                                    #
# 8. Should gays hold public positions?                                                        #
# 9. Should a female homosexual couple be allowed to adopt children?                           #
# 10. Should a male homosexual couple be allowed to adopt children?                            #
################################################################################################


# Import Data
SexAtt <- read.table("http://academia.edu.documents.s3.amazonaws.com/90840/sexualat.dat")
names(SexAtt) <- c("divorce", "sexdisc", "premar", "exmar", "gaysex", 
"gayscho", "gayhied", "gaypubl", "gayfadop", "gaymadop")

head(SexAtt)


# install the latest version of the `ltm' package
# (if you do not already have it)
install.packages("ltm", dependencies = TRUE)

# load `ltm' package
library(ltm)



##########################
# Descriptive Statistics #
##########################

dsc <- descript(SexAtt)
dsc
plot(dsc, type = "b", lty = 1, pch = 1:10, col = 1:10, lwd = 2, cex = 1.1, xlim = c(-0.5, 9))
legend("left", names(SexAtt), pch = 1:10, col = 1:10, lty = 1, lwd = 2, cex = 1.1, bty = "n")



####################
# Fit Rasch Models #
####################

# fix discrimination to 1
fit.rasch1 <- rasch(SexAtt, constraint = cbind(ncol(SexAtt) + 1, 1))
summary(fit.rasch1)
# items ordered by difficulty, and
# probability of positive response by the average
# individual
coef(fit.rasch1, prob = TRUE, order = TRUE)


# unconstrained Rasch model
fit.rasch2 <- rasch(SexAtt)
summary(fit.rasch2)
# items ordered by difficulty, and
# probability of positive response by the average
# individual
coef(fit.rasch2, prob = TRUE, order = TRUE)


# check the residuals for all observed response
# patterns; by default response patterns are ordered
# according to their residual values
residuals(fit.rasch1)
residuals(fit.rasch2)


# check the residuals for specific response
# patterns
patterns <- rbind("all.zeros" = rep(0, 10), 
    "mix1" = rep(0:1, length = 10),
    "mix2" = rep(1:0, length = 10),
    "all.ones" = rep(1, 10))
residuals(fit.rasch1, resp.patterns = patterns, order = FALSE)
residuals(fit.rasch2, resp.patterns = patterns, order = FALSE)


# check the fit to the two-way margins
margins(fit.rasch1)
margins(fit.rasch2)


# check the fit to the three-way margins
margins(fit.rasch1, type = "three-way")
margins(fit.rasch2, type = "three-way")


# Bootstrapped Pearson chi-squared goodness-of-fit tests
GoF.rasch(fit.rasch1, B = 100)
GoF.rasch(fit.rasch2, B = 100)


# perform a Likelihood Ratio Test between the two models
anova(fit.rasch1, fit.rasch2)


# Item Characteristic Curves for the two models
plot(fit.rasch1, legend = TRUE, pch = rep(1:2, each = 5), xlab = "Attitude",
     col = rep(1:5, 2), lwd = 2, cex = 1.2, sub = paste("Call: ", deparse(fit.rasch1$call)))
plot(fit.rasch2, legend = TRUE, pch = rep(1:2, each = 5), xlab = "Attitude",
     col = rep(1:5, 2), lwd = 2, cex = 1.2, sub = paste("Call: ", deparse(fit.rasch2$call)))


# Item Information Curves for the two models
plot(fit.rasch1, type = "IIC", legend = TRUE, pch = rep(1:2, each = 5), xlab = "Attitude",
     col = rep(1:5, 2), lwd = 2, cex = 1.2, sub = paste("Call: ", deparse(fit.rasch1$call)))
plot(fit.rasch2, type = "IIC", legend = TRUE, pch = rep(1:2, each = 5), xlab = "Attitude",
     col = rep(1:5, 2), lwd = 2, cex = 1.2, sub = paste("Call: ", deparse(fit.rasch2$call)))


# Test Information Functions for the two models
info1 <- plot(fit.rasch1, type = "IIC", items = 0, lwd = 2, xlab = "Attitude")
info2 <- plot(fit.rasch2, type = "IIC", items = 0, lwd = 2, xlab = "Attitude")
# put them in the same plot
plot(range(info1[, "z"]), range(info1[, "info"], info2[, "info"]), type = "n",
     xlab = "Attitude", ylab = "Information", main = "Test Information Functions")
lines(info1, lwd = 2)
lines(info2, lwd = 2, lty = 2)
legend("topleft", c("Rasch model with discrimination equal to 1", "Rasch model"), lty = 1:2, lwd = 2, bty = "n")


# Standard Error of Measurement for the two models
plot(info1[, "z"], 1 / sqrt(info1[, "info"]), type = "l", lwd = 2, xlab = "Attitude", ylab = "Standard Error", 
     main = "Standard Error of Measurement")
plot(info2[, "z"], 1 / sqrt(info2[, "info"]), type = "l", lwd = 2, xlab = "Attitude", ylab = "Standard Error", 
     main = "Standard Error of Measurement")
# put them in the same plot
plot(range(info1[, "z"]), range(1 / sqrt(info1[, "info"]), 1 / sqrt(info2[, "info"])), type = "n",
     xlab = "Attitude", ylab = "Standard Error", main = "Standard Error of Measurement")
lines(info1[, "z"], 1 / sqrt(info1[, "info"]), lwd = 2)
lines(info2[, "z"], 1 / sqrt(info2[, "info"]), lwd = 2, lty = 2)
legend("right", c("Rasch model with discrimination equal to 1", "Rasch model"), lty = 1:2, lwd = 2, bty = "n")


# the Rasch model under the normal ogive can fitted
# by fixing the discrimination parameter to 1.702
fit.rasch3 <- rasch(SexAtt, constraint = cbind(ncol(SexAtt) + 1, 1.702))
summary(fit.rasch3)



########################################
# Fit the Two-Parameter Logistic Model #
########################################

fit.2pl <- ltm(SexAtt ~ z1)
summary(fit.2pl)
coef(fit.2pl, standardized = TRUE, prob = TRUE, order = TRUE)


# check the fit to the two-way margins
margins(fit.2pl)


# check the fit to the three-way margins
margins(fit.2pl, type = "three-way")


# perform a LRT with the unconstrained Rasch model
anova(fit.rasch2, fit.2pl)


# Item Characteristic Curves
plot(fit.2pl, legend = TRUE, pch = rep(1:2, each = 5), xlab = "Attitude",
     col = rep(1:5, 2), lwd = 2, cex = 1.2, sub = paste("Call: ", deparse(fit.2pl$call)))


# Item Information Curves
plot(fit.2pl, type = "IIC", legend = TRUE, pch = rep(1:2, each = 5), xlab = "Attitude",
     col = rep(1:5, 2), lwd = 2, cex = 1.2, sub = paste("Call: ", deparse(fit.2pl$call)))


# Item Information Curves, excluding "gayscho" and "gayhied"
plot(fit.2pl, type = "IIC", items = which(!names(SexAtt) %in% c("gayscho", "gayhied")), 
     legend = TRUE, pch = rep(1:2, each = 5), col = rep(1:5, 2), lwd = 2, cex = 1.2, 
     sub = paste("Call: ", deparse(fit.2pl$call)), xlab = "Attitude")


# Test Information Function
info2pl <- plot(fit.2pl, type = "IIC", items = 0, lwd = 2, xlab = "Attitude")

# put in one plot
plot(range(info1[, "z"]), range(info1[, "info"], info2[, "info"], info2pl[, "info"]), type = "n",
     xlab = "Attitude", ylab = "Information", main = "Test Information Functions")
lines(info1, lwd = 2)
lines(info2, lwd = 2, lty = 2)
lines(info2pl, lwd = 2, lty = 3)
legend("topleft", c("Rasch model with discrimination equal to 1", "Rasch model", "Two-Parameter Logistic model"), 
    lty = 1:3, lwd = 2, bty = "n")


# Standard Error of Measurement
plot(info2pl[, "z"], 1 / sqrt(info2pl[, "info"]), type = "l", lwd = 2, xlab = "Attitude", ylab = "Standard Error", 
     main = "Standard Error of Measurement")



##################################################
# Fit Latent Trait Model with 2 latent variables #
##################################################

# use 21 quadrature points instead of the default 15
fit.lt2l <- ltm(SexAtt ~ z1 + z2, control = list(GHk = 21))
summary(fit.lt2l)
coef(fit.lt2l)


# check the fit to the two-way margins
margins(fit.lt2l)


# check the fit to the three-way margins
margins(fit.lt2l, type = "three-way")


# perform a LRT with the 2PL model
anova(fit.2pl, fit.lt2l)


# Standardized Loadings scatterplot
plot(fit.lt2l, type = "loadings")


# Item Characteristic Surfaces
plot(fit.lt2l, ticktype = "detailed", theta = 30, phi = 30, expand = 0.5, d = 2, 
     cex = 0.7, col = "lightblue")


# items 6 (`gayscho'), 7 (`gayhied') and 10 (`gaymadop')
# seem to perform differently than the other items; we 
# perform an analysis excluding them
fit.lt2l.2 <- ltm(SexAtt[c(1:5, 8:9)] ~ z1 + z2, control = list(GHk = 21))


# the fit on margins is better
margins(fit.lt2l.2)
margins(fit.lt2l.2, type = "three-way")



###############################################
# Fit Latent Trait Model with nonlinear terms #
###############################################

# include an interaction term between the 2 latent variables;
# use 21 quadrature points instead of the default 15
fit.ltnl <- ltm(SexAtt ~ z1 * z2, control = list(GHk = 21))
summary(fit.ltnl)
coef(fit.ltnl)


# check the fit to the two-way margins
margins(fit.ltnl)


# check the fit to the three-way margins
margins(fit.ltnl, type = "three-way")


# perform a LRT with the linear 2-factor model
anova(fit.lt2l, fit.ltnl)
