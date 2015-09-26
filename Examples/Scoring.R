######################################
# Factor Scoring - Ability Estimates #
######################################


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



#########################################################
# Posterior modes as ability estimates under each model #
#########################################################

factor.scores(fitRasch.c)
factor.scores(fitRasch)
factor.scores(fit2PL)
factor.scores(fit3PL.rc)
factor.scores(fit3PL.r)
factor.scores(fit3PL)


# by default factor.scores() produces ability estimates for
# all the observed response patterns. If you wish to obtain
# ability estimates for specific patterns, then you can use
# the `resp.patterns' argument, e.g.,
factor.scores(fitRasch.c, resp.patterns = rbind(c(1,0,1,0,1)))
factor.scores(fitRasch, resp.patterns = rbind(c(1,0,1,0,1)))
factor.scores(fit2PL, resp.patterns = rbind(c(1,0,1,0,1)))
factor.scores(fit3PL.rc, resp.patterns = rbind(c(1,0,1,0,1)))
factor.scores(fit3PL.r, resp.patterns = rbind(c(1,0,1,0,1)))
factor.scores(fit3PL, resp.patterns = rbind(c(1,0,1,0,1)))



##################################################
# Kernel Density Estimation of Ability Estimates #
##################################################

# Extract ability estimates for each observed response pattern
theta.Rasch.c <- factor.scores(fitRasch.c)
theta.Rasch <- factor.scores(fitRasch)
theta.2PL <- factor.scores(fit2PL)
theta.3PL.rc <- factor.scores(fit3PL.rc)
theta.3PL.r <- factor.scores(fit3PL.r)
theta.3PL <- factor.scores(fit3PL)


# the plots; the `include.items' argument
# superimposes the item parameters
par(mfrow = c(3, 2))
plot(theta.Rasch.c, include.items = TRUE, main = "Rasch Model (discr = 1)")
plot(theta.Rasch, include.items = TRUE, main = "Rasch Model")
plot(theta.2PL, include.items = TRUE, main = "Two-Parameter Logistic Model")
plot(theta.3PL.rc, include.items = TRUE, main = "Three-Parameter Model (discr = 1)")
plot(theta.3PL.r, include.items = TRUE, main = "Three-Parameter Model (discr equal across items)")
plot(theta.3PL, include.items = TRUE, main = "Three-Parameter Model")


# If you want to put everything in one plot, you can do
# it manually using the following code

# Extract ability estimates for each observed response pattern
theta.Rasch.c <- factor.scores(fitRasch.c)$score.dat$z1
theta.Rasch <- factor.scores(fitRasch)$score.dat$z1
theta.2PL <- factor.scores(fit2PL)$score.dat$z1
theta.3PL.rc <- factor.scores(fit3PL.rc)$score.dat$z1
theta.3PL.r <- factor.scores(fit3PL.r)$score.dat$z1
theta.3PL <- factor.scores(fit3PL)$score.dat$z1

# Extract observed frequency for each response pattern
Obs <- factor.scores(fitRasch.c)$score.dat$Obs


# Compute Kernel Density Estimates
bw <- 0.25 # you can play with various choices for the bandwidth
d.Rasch.c <- density(rep(theta.Rasch.c, Obs), bw = bw)
d.Rasch <- density(rep(theta.Rasch, Obs), bw = bw)
d.2PL <- density(rep(theta.2PL, Obs), bw = bw)
d.3PL.rc <- density(rep(theta.3PL.rc, Obs), bw = bw)
d.3PL.r <- density(rep(theta.3PL.r, Obs), bw = bw)
d.3PL <- density(rep(theta.3PL, Obs), bw = bw)

xs <- cbind(d.Rasch.c$x, d.Rasch$x, d.2PL$x, d.3PL.rc$x, d.3PL.r$x, d.3PL$x)
ys <- cbind(d.Rasch.c$y, d.Rasch$y, d.2PL$y, d.3PL.rc$y, d.3PL.r$y, d.3PL$y)
matplot(xs, ys, xlab = "Ability", ylab = "Density", lty = 1, lwd = 2, type = "l")
legend("topleft", c("Rasch, discrimination = 1", "Rasch", "2PL", 
    "3PL, discrimination = 1", "3PL, discrimination equal across items", "3PL"), 
    lty = 1, lwd = 2, col = 1:6, bty = "n")



################################
# Expected A Posteriori Scores #
################################

factor.scores(fitRasch.c, method = "EAP")
factor.scores(fitRasch, method = "EAP")
factor.scores(fit2PL, method = "EAP")
factor.scores(fit3PL.rc, method = "EAP")
factor.scores(fit3PL.r, method = "EAP")
factor.scores(fit3PL, method = "EAP")



####################
# Component Scores #
####################

# factor.scores() can also compute the Component Scores
# for `ltm' objects, as have been proposed by
# Bartholomew (1984, RSSB, 46, 120–123)
factor.scores(fit2PL, method = "Component")
factor.scores(fit2PL, method = "Component", resp.patterns = rbind(c(1,0,1,0,1)))
