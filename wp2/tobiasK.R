# tobiasK.R

# Experimenting with Econ 407 from Evernote rlhick.people.wm.edu

library(foreign)
library(sandwich)
library(lmtest)
library(boot)
library(AER)
library(car)
library(ivpack)
tk.df = read.dta("https://rlhick.people.wm.edu/econ407/data/tobias_koop.dta")
tk4.df = subset(tk.df, time == 4)
attach(tk4.df)

# OLS
(ols.lm = lm(ln_wage ~ pexp + pexp2 + broken_home + educ))

# IV

ivmodel <- ivreg(ln_wage ~ pexp + pexp2 + broken_home + educ |
                   pexp + pexp2 + broken_home + feduc)
summary(ivmodel)

summary(ivmodel,vcov=sandwich) # Huber-White errors

summary(ivmodel,vcov=sandwich, diagnostics = TRUE) 

# generates test for i) weak instruments; ii) Wu-Hausman 

# Very low p value ==> we cannot reject null of valid instruments