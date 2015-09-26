################################################################################################
# Sample analysis for the Environment data-set.                                                #
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

# load `ltm' package
library(ltm)


##########################
# Descriptive Statistics #
##########################

descript(Environment)

rcor.test(Environment, method = "kendall")



#####################################
# constrained Graded Response Model #
#####################################

# the constrained Graded Response Model assumes a common
# discrimination parameter across items;
# `Hessian = TRUE' is used to obtain standard errrors
fit1 <- grm(Environment, constrained = TRUE, Hessian = TRUE)
fit1
summary(fit1)


# check the residuals
residuals(fit1)


# check the fit on the margins
margins(fit1)
margins(fit1, type = "three-way")



#######################################
# unconstrained Graded Response Model #
#######################################

# the unconstrained Graded Response Model assumes a different
# discrimination parameter per item;
# `Hessian = TRUE' is used to obtain standard errrors
fit2 <- grm(Environment, Hessian = TRUE)
fit2
summary(fit2)


# check the residuals
residuals(fit2)


# check the fit on the margins
margins(fit2)
margins(fit2, type = "three-way")


# Likelihood Ratio Test between constrained 
# and unconstrained models
anova(fit1, fit2)



##############################################
# Item Charecteristic and Information Curves #
##############################################

# by default the Item Response Category Characterstic
# Curves are produced; we include a legend
par(mfrow = c(2, 2))
plot(fit2, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# for the GRM the Item Operation Characteristic Curves can be fittes
# as well by specifying `type = "OCCu"' or `type = "OCCl"'
par(mfrow = c(2, 2))
plot(fit2, type = "OCCu", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# the Item Information Curves are produced by specifying
# `type = "IIC"'
plot(fit2, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# for the Test Information Curve you need to specify 'items = 0'
plot(fit2, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)



# The Item Response Category Characteristic Curves can be plotted
# for each response category separately as follows
par(mfrow = c(2, 2))
plot(fit2, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5,
     cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3,
     cex.axis = 1.1)
for (ctg in 2:3) {
      plot(fit2, category = ctg, lwd = 2, cex = 1.2, annot = FALSE,
           xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3,
           cex.axis = 1.1)
}



###################################
# Operating Charecteristic Curves #
###################################

# the Operating Characterstic Curves are produced 
# by specifying `type = "OCCu"' or `type = "OCCl"'
par(mfrow = c(2, 2))
plot(fit2, type = "OCCu", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


# Combine all Operating Characterstic Curves in one plot:
# by specifying `plot = FALSE' the plot is not produced 
# but you extract the values used to create it
vals <- plot(fit2, type = "OCCu", plot = FALSE)

# now create the plot
plot(range(vals$z), 0:1, type = "n", xlab = "Latent Trait", ylab = "Probability",
     main = "Operation Characteristic Curves")

# define the colours you would like to use and plot
cols <- c("black", "red", "blue", "cyan", "green3", "magenta")
for (i in seq_along(cols)) {
    matlines(vals$z, vals$pr[[i]], col = cols[i], lwd = 2)
}

# include a legend
labs <- paste(rep(names(Environment), 2), "-",
        rep(c("slightly concerned", "not very concerned"), each = 6))
legend("topleft", labs, col = rep(cols, 2),
        lty = rep(1:2, each = 6), lwd = 2, bty = "n")
