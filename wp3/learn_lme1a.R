# learn_lme1a.R

# https://www.jaredknowles.com/journal/2013/11/25/getting-started-with-mixed-effect-models-in-r

library(lme4) # load library
library(arm) # convenience functions for regression in R
lmm.data <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#summary(lmm.data)
head(lmm.data)

OLSexamp <- lm(extro ~ open + agree + social, data = lmm.data)
display(OLSexamp)

MLexamp <- glm(extro ~ open + agree + social, data=lmm.data)
display(MLexamp)

MLexamp.2 <- glm(extro ~ open + agree + social + class, data=lmm.data )
display(MLexamp.2)

anova(MLexamp, MLexamp.2, test="F")
glance(MLexamp.2)

MLexamp.3 <- glm(extro ~ open + agree + social + school, data=lmm.data )
display(MLexamp.3)

anova(MLexamp, MLexamp.3, test="F")

table(lmm.data$school, lmm.data$class)

MLexamp.4 <- glm(extro ~ open + agree + social + school:class, data=lmm.data )
display(MLexamp.4)

MLexamp.5 <- glm(extro ~ open + agree + social + school*class - 1, data=lmm.data )
display(MLexamp.5)


MLexamp.6 <- lmer(extro ~ open + agree + social + (1|school), data=lmm.data)
display(MLexamp.6)

MLexamp.7 <- lmer(extro ~ open + agree + social + (1|school) + (1|class), data=lmm.data)
display(MLexamp.7)

MLexamp.8 <- lmer(extro ~ open + agree + social + (1|school/class), data=lmm.data)
display(MLexamp.8)

MLexamp.9 <- lmer(extro ~ open + agree + social + (1+open|school/class), data=lmm.data)
display(MLexamp.9)

print(sessionInfo(),locale=FALSE)

#########################
# https://www.jaredknowles.com/journal/2014/5/17/mixed-effects-tutorial-2-fun-with-mermod-objects
str(lmm.data)


# Explore data
require(lattice)
xyplot(extro ~ open + social + agree | class, data = lmm.data, 
       auto.key = list(x = .85, y = .035, corner = c(0, 0)), 
       layout = c(4,1), main = "Extroversion by Class")



xyplot(extro ~ open + social + agree | school, data = lmm.data, 
       auto.key = list(x = .85, y = .035, corner = c(0, 0)), 
       layout = c(3, 2), main = "Extroversion by School")


xyplot(extro ~ open + social + agree | school + class, data = lmm.data, 
       auto.key = list(x = .85, y = .035, corner = c(0, 0)), 
       main = "Extroversion by School and Class")


MLexamp1 <- lmer(extro ~ open + agree + social + (1|school), data=lmm.data)
class(MLexamp1)

# S4 class

slotNames(MLexamp1)
# [1] "resp"    "Gp"      "call"    "frame"   "flist"   "cnms"    "lower"  
# [8] "theta"   "beta"    "u"       "devcomp" "pp"      "optinfo"


MLexamp1@call # returns the model call
MLexamp1@optinfo
methods(class="merMod") # provides 70 methods
methods(class = "merMod","ranef") # 7 kinds of ranef

ranef(MLexamp1) # only values 

re1 <- ranef(MLexamp1, condVar=TRUE) # save the ranef.mer object
class(re1) # is ranef.mer, which did not show as one of 7 in methods()
attr(re1[[1]], which = "postVar")
