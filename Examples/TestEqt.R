#####################################################
# Test equating by common Items and common Subjects #
#####################################################


# install the latest version of the `ltm' package
# (if you do not already have it)
install.packages("ltm", dependencies = TRUE)

# load `ltm' package
library(ltm)



###########################
# Alternate Form Equating #
###########################

# let 4 forms of items `dat1', `dat2', `dat3'
# & `dat4'. The common items in all forms are
# `CIt3' and `CIt4'
set.seed(1234)

dat1 <- as.data.frame(rmvlogis(150, cbind(c(-2, 1, 2, 1), 1)))
names(dat1) <- c("CIt2", "CIt3", "CIt4", "W")

dat2 <- as.data.frame(rmvlogis(110, cbind(c(-2, -1, 1, 2, 0.95), 1)))
names(dat2) <- c("CIt1", "CIt2", "CIt3", "CIt4", "K")

dat3 <- as.data.frame(rmvlogis(90, cbind(c(-2, 1, 2), 1)))
names(dat3) <- c("CIt1", "CIt3", "CIt4")

dat4 <- as.data.frame(rmvlogis(150, cbind(c(-2, -1, 1, 2, 1.05), 1)))
names(dat4) <- c("CIt1", "CIt2", "CIt3", "CIt4", "V1")


# First combine the 4 forms in one data-set
lisForms <- list(dat1, dat2, dat3, dat4)
datAll <- testEquatingData(lisForms)


# Fit a Rasch model in the combined data-set;
# the summary() method points out the common items
fit <- rasch(datAll, constraint = cbind(ncol(datAll) + 1, 1))
summary(fit)



##########################
# Across Sample Equating #
##########################

#let 4 forms `Dat1', `Dat2', `Dat3' & `Dat4', 
# with unique items; `AnchIts' is the form 
# containing the anchoring items
set.seed(654321)

Dat1 <- as.data.frame(rmvlogis(150, cbind(seq(-2, 2, len = 3), 1.5)))
names(Dat1) <- sample(LETTERS, 3)

Dat2 <- as.data.frame(rmvlogis(150, cbind(seq(-2.5, 2.5, len = 3), 0.7)))
names(Dat2) <- sample(letters, 3)

Dat3 <- as.data.frame(rmvlogis(150, cbind(seq(-1, 1, len = 3), 1.2)))
names(Dat3) <- c("XY", "ZW", "KL")

Dat4 <- as.data.frame(rmvlogis(150, cbind(seq(-1.5, 1.5, len = 3), 1.9)))
names(Dat4) <- paste("D", 1:3, sep = "")

AnchIts <- as.data.frame(rmvlogis(150, cbind(c(-0.5, 1, 1.5, 2), 1)))


# First combine the 4 forms to be equated with the 
# anchoring items in one data-set
lisForms <- list(Dat1, Dat2, Dat3, Dat4)
datAll <- testEquatingData(lisForms, AnchoringItems = AnchIts)


# Fit a Rasch model in the combined data-set;
# fix the anchoring items difficulty parameters, using
# the `constraint' argument, however, remember that 
# `constraint' accepts parameters with the additive
# parameterization, check ?rasch() for more info;
# the summary() method points out the anchoring items
difficulties <- c(-0.5, 1, 1.5, 2)
constr <- cbind(c(1:4, ncol(datAll) + 1), c(-difficulties, 1))
constr
fit <- rasch(datAll, constraint = constr)
summary(fit)


# a two parameter logistic model;
# fix the difficulty and discrimination parameters for
# the anchoring items, which are the first 4 items;
# check ?ltm() for more info on how to use the `constraint'
# argument
difficulties <- c(-0.5, 1, 1.5, 2)
items <- rep(1:4, 2)
parms <- rep(1:2, each = 4)
vals <- c(-difficulties, rep(1, 4))
constr <- cbind(items, parms, vals)
constr
fit <- ltm(datAll ~ z1, constraint = constr)
summary(fit)



############################
# Common Subjects Equating #
############################

# let 3 practice exams and the true exam
# taken by the same 500 individuals
set.seed(123321)


# simulate ability estimates of the
# 500 subjects
ablts <- rnorm(500)


# simulate data
pract1 <- as.data.frame(rmvlogis(500, cbind(seq(-2, 2, len = 10), 1.5), z.vals = ablts))
names(pract1) <- sample(LETTERS, 10)

pract2 <- as.data.frame(rmvlogis(500, cbind(seq(-2, 2, len = 10), 1.4), z.vals = ablts))
names(pract2) <- letters[1:10]

pract3 <- as.data.frame(rmvlogis(500, cbind(seq(-2, 2, len = 10), 1.6), z.vals = ablts))
names(pract3) <- letters[11:20]

exam <- as.data.frame(rmvlogis(500, cbind(seq(-2, 2, len = 10), 1.5), z.vals = ablts))
names(exam) <- paste("Q", 1:10, sep = "")


# Equate subjects using the Rasch model
fit.pract1 <- rasch(pract1)
fit.pract2 <- rasch(pract2)
fit.pract3 <- rasch(pract3)
fit.exam <- rasch(exam)


# Obtain ability estimates
ablt.pract1 <- factor.scores(fit.pract1, resp.patterns = pract1)$score.dat
ablt.pract2 <- factor.scores(fit.pract2, resp.patterns = pract2)$score.dat
ablt.pract3 <- factor.scores(fit.pract3, resp.patterns = pract3)$score.dat
ablt.exam <- factor.scores(fit.exam, resp.patterns = exam)$score.dat


# combine in one data-set; if necessary order the ability estimates
# by subjects id
ablt.dat <- rbind("Ablt.Prct1" = ablt.pract1$z1, 
    "Ablt.Prct2" = ablt.pract2$z1,
    "Ablt.Prct3" = ablt.pract3$z1,
    "Ablt.Exam" = ablt.exam$z1)


# make a scatterplot for the first 5 subjects
matplot(ablt.dat[, 1:5], type = "b", lty = 1, col = "black", 
        ylab = "Ability", xaxt = "n")
axis(1, at = 1:4, c(paste("Pract", 1:3), "Exam"))
