################################################################################################
# Sample analysis for the Environment data set.                                                #
# Description: The data set is extracted from the 1990 British Social Attitudes Survey.        #
# A 291 individuals were asked about their opinion on six environmental issues. The response   #
# options were 'very concerned', 'slightly concerned' and 'not very concerned' giving thus     #
# rise to the following six ordinal items:                                                     #
# 1. 'LeadPetrol': Lead from petrol                                                            #
# 2. 'RiverSea': River and sea pollution                                                       #
# 3. 'RadioWaste': Transport and storage of radioactive waste                                  #
# 4. 'AirPollution': Air pollution                                                             #
# 5. 'Chemicals': Transport and disposal of poisonous chemicals                                #
# 6. 'Nuclear': Risks from nuclear power station                                               #
################################################################################################


# install the latest version of the `ltm' package
# (if you do not already have it)
install.packages("ltm", dependencies = TRUE)

# load `ltm'
library(ltm)


##########################
# Descriptive Statistics #
##########################

# check how the first 10 rows of the data set look like
head(Environment, 10)

# frequencies per response category per item
summary(Environment)

# IRT-specific descriptive statistics can be produced
# using the descript() function
descript(Environment)

# kendall tau between pairs of items
rcor.test(Environment, method = "kendall")
# you can also adjust for multple comparisons
rcor.test(Environment, method = "kendall", p.adjust = TRUE)



##########################################
# Rasch Generalized Partial Credit Model #
##########################################

# the Rasch version of the Generalized Partial Credit Model 
# assumes a common discrimination parameter across items fixed at 1
fit1 <- gpcm(Environment, constraint = "rasch")
fit1
summary(fit1)
# you can ask for sandwich standard errors using
# the `robust.se' argument
summary(fit1, robust.se = TRUE)


# check the fit on the margins
margins(fit1)
margins(fit1, type = "three-way")

# the fit can also be assessed using the
# Pearson chi-squared goodness-of-fit measure;
# this can be done using the GoF.gpcm() function.
# By default the function produces an estimate to the
# p-value using a parametetric Boostrap approach.
# This is safer than using the asymptotic chi-squared
# distribution but it requires more computing time.
# In the first call below we use `simulate.p.value = FALSE'
# the specifies to use the asymptotic distribution.
# In the second call the Bootstrap approach is used.
GoF.gpcm(fit1, simulate.p.value = FALSE)
GoF.gpcm(fit1, B = 199) # this will take some time depending on your machine



########################################
# 1PL Generalized Partial Credit Model #
########################################

# the 1PL version of the Generalized Partial Credit Model 
# assumes a common discrimination parameter across items
fit2 <- gpcm(Environment, constraint = "1PL")
fit2
summary(fit2)


# check the fit on the margins
margins(fit2)
margins(fit2, type = "three-way")

# you can also use the GoF.gpcm() function
GoF.gpcm(fit2, simulate.p.value = FALSE)



####################################
# Generalized Partial Credit Model #
####################################

# the unconstrained version of the Generalized Partial Credit 
# assumes a different discrimination parameter per item;
fit3 <- gpcm(Environment)
fit3
summary(fit3)


# for each observed response pattern under the model:

# expected frequencies
fitted(fit3) 
# expected marginal probabilities
fitted(fit3, type = "marginal")
# expected conditional (on the latent ability) probabilities
fitted(fit3, type = "conditional")

# you can also compute the fitted probabilities for specific patterns, e.g.,
fitted(fit3, resp.patterns = cbind(1,2,3,NA,2,1))
fitted(fit3, resp.patterns = rbind("A" = c(1,2,3,NA,2,1), "B" = c(1,1,1,2,2,2)))


# the same options are also available for the residuals residuals values
residuals(fit3)


# check the fit on the margins
margins(fit3)
margins(fit3, type = "three-way")


# Likelihood Ratio Test between constrained and unconstrained models;
# here we test if the discrimination parameter should be fixed at 1
anova(fit1, fit2)
# here we test if the discrimination parameter is equal among the 6 items
anova(fit2, fit3)

# in cases required, you can use a parametric Bootstrap approach
# to estimate the p-value of the LRT, e.g.,
av <- anova(fit1, fit2, simulate.p.value = TRUE) # this will take some time depending on your machine
av
plot(av) # simulated distribution of the LRT (i.e., we compare with the asymptotic chi-squared distribution under the null)


#################
# Factor Scores #
#################

# the latent abilities can be estimated using
# the factor.scores() function:

# default is maximum a posteriori
fs <- factor.scores(fit3)
fs
# you can also plot the kernel density estimator for
# the latent abilities using the plot method, e.g.,
plot(fs)

# for expected a posteriori use
factor.scores(fit3, method = "EAP")

# factor scores can also be computed for specific response patterns;
# to do that use the `resp.patterns' argument, e.g.,
factor.scores(fit3, resp.patterns = rbind("A" = c(1,2,3,NA,2,1), "B" = c(1,1,1,2,2,2)))




##############################################
# Item Charecteristic and Information Curves #
##############################################

# by default the Item Response Category Characterstic
# Curves are produced; we include a legend
par(mfrow = c(2, 2))
plot(fit3, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

# if you want them for a specific item, then use the `items'
# argument, e.g., here for the third item
plot(fit3, items = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# for the GPCM the Item Operation Characteristic Curves can be fitted
# as well by specifying `type = "OCCu"' or `type = "OCCl"'
par(mfrow = c(2, 2))
plot(fit3, type = "OCCu", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# the Item Information Curves are produced by specifying
# `type = "IIC"'
plot(fit3, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# for the Test Information Curve you need to specify `items = 0'
plot(fit3, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# compare the Test Information Curve of the three version of GPCM
# (check the range of the y-axis in each plot)
par(mfrow = c(2, 2))
plot(fit1, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     main = "Rasch Version", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
#
plot(fit2, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     main = "1PL Version", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
#
plot(fit3, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     main = "GPCM Version", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# to see more clearly the difference in the amount of infromation provided by
# the three models use the following code. In addition, we have also
# included the information from the Graded Response Model for a comparison:
vals1 <- plot(fit1, type = "IIC", items = 0, plot = FALSE)
vals2 <- plot(fit2, type = "IIC", items = 0, plot = FALSE)
vals3 <- plot(fit3, type = "IIC", items = 0, plot = FALSE)
vals4 <- plot(fit4 <- grm(Environment, constrained = TRUE), type = "IIC", items = 0, plot = FALSE)
vals5 <- plot(fit5 <- grm(Environment), type = "IIC", items = 0, plot = FALSE)

values <- cbind(vals1[, "test.info"], vals2[, "test.info"], vals3[, "test.info"], vals4[, "test.info"], vals5[, "test.info"])
matplot(vals1[, "z"], values, type = "l", col = c("black", "red", "blue", "green", "goldenrod1"), 
    xlab = "Latent Trait", ylab = "Information", main = "A Comparison between Different Polytomous IRT Models", 
    lty = 1, lwd = 2, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
lgd <- c("GPCM (discrimination = 1)", "GPCM (discrimination equal among items)", 
    "GPCM (discrimination different per item)", "GRM (discrimination equal among items)", "GRM (discrimination different per item)")
legend("topleft", lgd, col = c("black", "red", "blue", "green", "goldenrod1"), cex = 1.2, lty = 1, lwd = 2, bty = "n")


# using the function information() you can numerically compare
# the infromation provided by the different IRT models for 
# polytomous data

# discrimination = 1
information(fit1, c(-4, 4))

# discrimination equal among items
information(fit2, c(-4, 4))
information(fit4, c(-4, 4))

# discrimination different per item
information(fit3, c(-4, 4))
information(fit5, c(-4, 4))
