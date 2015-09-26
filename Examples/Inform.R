#############################
# Item and Test Information #
#############################


# install the latest version of the `ltm' package
# (if you do not already have it)
install.packages("ltm", dependencies = TRUE)

# load `ltm' package
library(ltm)



#####################################################
# Fit the Rasch and 2PL models to the LSAT data-set #
#####################################################

fitRasch <- rasch(LSAT, constraint = cbind(ncol(LSAT) + 1, 1))
fit2PL <- ltm(LSAT ~ z1)



###############################################################
# The information() function computes the area under the Test #
# or Item Information Curves; it can be used to quantify the  #
# amount of information in specified intervals                #
###############################################################

# information for latent trait values in (0, 4),
# under the Rasch and 2PL (i.e., two-parameter logistic) models
information(fitRasch, c(0, 4))
information(fit2PL, c(0, 4))


# the amount of information specific items contribute
# can be extracted using the `items' argument, e.g.,
# the same as above but using only items 2 and 4
information(fitRasch, c(0, 4), items = c(2, 4))
information(fit2PL, c(0, 4), items = c(2, 4))



###########################
# combine all in one plot #
###########################

# first extract the value used to creat the plot by
# (the `plot = FALSE' argument specifies not to plot
# the test information curves)
plotRasch <- plot(fitRasch, items = 0, type = "IIC", plot = FALSE)
plot2PL <- plot(fit2PL, items = 0, type = "IIC", plot = FALSE)


# compute info under the Test Information Curves in the
# interval (-4, -2), (-2, 0), (0, 2) and (2, 4)
infoRasch1 <- information(fitRasch, c(-4, -2))
info2PL1 <- information(fit2PL, c(-4, -2))
infoRasch2 <- information(fitRasch, c(-2, 0))
info2PL2 <- information(fit2PL, c(-2, 0))
infoRasch3 <- information(fitRasch, c(0, 2))
info2PL3 <- information(fit2PL, c(0, 2))
infoRasch4 <- information(fitRasch, c(2, 4))
info2PL4 <- information(fit2PL, c(2, 4))


# the combined plot
plot(range(plotRasch[, "z"]), range(plotRasch[, "info"], plot2PL[, "info"]), type = "n", 
     xlab = "Ability", ylab = "Information", main = "Test Information Curves")
lines(plotRasch, lwd = 2)
lines(plot2PL, lwd = 2, lty = 2)
##
abline(v = c(-2, 0, 2), lty = 3)
text(-3.3, 0.2, paste("Rasch: ", round(infoRasch1$InfoRange, 2), 
     " (", round(100 * infoRasch1$PropRange, 2), "%)\n   2PL: ",
     round(info2PL1$InfoRange, 2), " (", round(100 * info2PL1$PropRange, 2), "%)", sep = ""))
#
text(-1.1, 0.2, paste("Rasch: ", round(infoRasch2$InfoRange, 2), 
     " (", round(100 * infoRasch2$PropRange, 2), "%)\n   2PL: ",
     round(info2PL2$InfoRange, 2), " (", round(100 * info2PL2$PropRange, 2), "%)", sep = ""))
#
text(1, 0.15, paste("Rasch: ", round(infoRasch3$InfoRange, 2), 
     " (", round(100 * infoRasch3$PropRange, 2), "%)\n   2PL: ",
     round(info2PL3$InfoRange, 2), " (", round(100 * info2PL3$PropRange, 2), "%)", sep = ""))
#
text(3, 0.2, paste("Rasch: ", round(infoRasch4$InfoRange, 2), 
     " (", round(100 * infoRasch4$PropRange, 2), "%)\n   2PL: ",
     round(info2PL4$InfoRange, 2), " (", round(100 * info2PL4$PropRange, 2), "%)", sep = ""))
##
legend("right", c("Rasch", "2PL"), lty = 1:2, lwd = 2, bty = "n")
