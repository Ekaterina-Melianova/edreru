# diebold1a.R
# A file to replicate from RLMS 1998 2008 2017, what Diebold does with CPS 1995 2004 and 2012
# Econometric Data Science Text from Diebold


options(scipen=999) # to supress scientific notation
Sys.setlocale("LC_ALL", "russian")

# Data: joined base
library(foreign)
library(dplyr)
library(stargazer)
library(gmodels)
library(lmtest)

library(questionr)
library(labelled)

memory.limit(1e10)
dat_joined <- foreign::read.spss(file="USER_RLMS-HSE_IND_1994_2018_v2.sav",
                                 use.value.labels = F,
                                 use.missings = F,
                                 to.data.frame = T)



s1a_98 <- dat_joined[which(dat_joined$YEAR==1998 & dat_joined$J13.2>0),
                     c("ID_I","ID_H","J13.2","EDUC","J5A","J5B","H7.2","H5","J23",
                       "I2","I4")]
# User-defined missing values
for (i in colnames(s1a_98)){
  na_values(s1a_98[,i]) <- 99999997:99999999
}


freq(s1a_98$J23)

summary(s1a_98$I2)
table(s1a_98$I2)


table(s1a_98$J23)
freq(s1a_98$J23)
summary(s1a_98$J23)
# I2 country of birth has 1638/3701 NAs - interesting fact but not usable easily
which(is.na(freq(s1a_98$I2)[,3])==T)
# I4 Nationality 3141 Russian only 9 missing will consider as non-Russian
freq(s1a_98$I4, total=T)
# J23 Is govt. owner 2615/914 quite amazing

# 
s1a_98$NON_RUSS[s1a_98$I4==1] <- 0
s1a_98$NON_RUSS[s1a_98$I4>1] <- 1
s1a_98$NON_RUSS[is.na(s1a_98$I4)] <- 1 # Only 9 added to 549 with given answers

CrossTable(s1a_98$NON_RUSS)


s1a_98$FEMALE[s1a_98$H5==2] <- 1
s1a_98$FEMALE[s1a_98$H5==1] <- 0

CrossTable(s1a_98$FEMALE)


s1a_98$PUB_SEC[s1a_98$J23==1] <- 1
s1a_98$PUB_SEC[s1a_98$J23==2] <- 0

CrossTable(s1a_98$PUB_SEC)

# Experience has to be constructed
# There is year and month information, 

# Let's calculate number of months of interview date, we know year is 1998
s1a_98$IMTHS <- (1997*12)+s1a_98$H7.2
# Let's calculate number of months of start date of 1st job - we will need to revise later for > 1 job
s1a_98$SWORK <- (s1a_98$J5A*12)+(s1a_98$J5B)

s1a_98$EXPER <- (s1a_98$IMTHS-s1a_98$SWORK)/12


## Adapted code from Diebold's CodeRWages.R

pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 2.5.pdf")
par(mfrow=c(2,2))
# Histograms for WAGE and LNWAGE ------------------------------------------

hist(s1a_98$J13.2, breaks=60, main="",col="turquoise")
hist(log(s1a_98$J13.2), breaks=40, main="",col="blue")


# Kernel Density Plot for WAGE and LNWAGE ---------------------------------

wage.density <- density(s1a_98$J13.2) #returns the density data for WAGE
wage.normal <- density(rnorm(1000000, mean=mean(s1a_98$J13.2), sd=sd(s1a_98$J13.2))) #returns the density data for normal distribution from the WAGE data
plot(wage.density, col="blue", main="", xlab="monthly Wage", ylab="Density",lwd=2) + points(wage.normal, type="l", col="red",lwd=2)


lnwage.density <- density(log(s1a_98$J13.2)) #returns the density data for LNWAGE
lnwage.normal <- density(rnorm(1000000, mean=mean(log(s1a_98$J13.2)), sd=sd(log(s1a_98$J13.2)))) #returns the density data for normal distribution from the LNWAGE data
plot(lnwage.density, col="blue", main="", xlab="Log monthly Wage", ylab="Density", ylim=c(0,.8),lwd=2) + points(lnwage.normal, type="l", col="red",lwd=2)

dev.off()


# Histograms for LNWAGE, EDUC, and EXPER ----------------------------------




pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 3.1.pdf")
par(mfrow=c(3,1))

#First Figure for LNWAGE Histogram
hist(log(s1a_98$J13.2), breaks=60, main="Log Monthly Wage",col="dodgerblue",xlab="Log Monthly Wages in Log Rubles",ylab="People")

#Second Figure for EDUC Histogram
hist(s1a_98$EDUC, breaks=40, main="Education",col="dodgerblue",
     xlab="Education in Years", ylab="People")

#Third Figure for EXPER Histogram
hist(s1a_98$EXPER, breaks=25, main="Experience",col="dodgerblue",xlab="Experience in Years",ylab="People")

dev.off()


# Scatterplot of EDUC vs. LNWAGE ------------------------------------------
pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 3.2.pdf",height=6,width=7.5)
plot(s1a_98$EDUC, log(s1a_98$J13.2), col="blue")
dev.off()


pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 3.3.pdf",height=6,width=7.5)
plot(s1a_98$EDUC, log(s1a_98$J13.2), col="blue") + abline(reg=lm(log(s1a_98$J13.2)~s1a_98$EDUC), col="red",
                                                          xlab="EDUC",ylab="LWAGE")
dev.off()
summary(lm(log(s1a_98$J13.2)~s1a_98$EDUC))


# Regress LNWAGE ~ EDUC, EXPER --------------------------------------------

# need a dataframe without missing values on any of the RHS vars
s1b_98 <- s1a_98 %>% filter(!is.na(EXPER) & !is.na(EDUC))


reg.lnwage.by.educ.exper <- lm(log(s1b_98$J13.2)~s1b_98$EDUC+s1b_98$EXPER)
summary(reg.lnwage.by.educ.exper)

stargazer(reg.lnwage.by.educ.exper,type="text")


pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 3.5.pdf",height=6,width=7.5)
fit.lnwage.by.educ.exper <- fitted.values(reg.lnwage.by.educ.exper)
plot(fit.lnwage.by.educ.exper, log(s1b_98$J13.2), col="blue", xlab="FIT", xlim=c(0,7), ylim=c(0,12))
dev.off()

#Creating residuals from Regression 1 predicted values
s1b_98$lnwage.actual <- log(s1b_98$J13.2)
s1b_98$lnwage.pred <- predict(reg.lnwage.by.educ.exper, s1b_98)
s1b_98$lnwage.resid <- s1b_98$lnwage.actual - s1b_98$lnwage.pred

#Plot of residuals and Fitted values superimposed on #residuals

pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 3.6.pdf",height=6,width=7.5)
plot(s1b_98$lnwage.actual, type="l", axes=FALSE, xlim=c(0,1300), ylim=c(-2,12), xlab="", ylab="", col="red") +
  lines(s1b_98$lnwage.pred, col="darkgreen") +
  lines(s1b_98$lnwage.resid, col="blue")
box()
axis(side=1, at=c(0,250,500,750,1000,1250))
axis(side=2, at=c(-2,-1,0,1,2))
axis(side=4, at=c(0,1,2,3,4,5,6,7,8,9,10,11,12))
par(xpd=TRUE)
legend(150, 5,inset=c(-0.5,0), legend=c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"), cex=0.7, horiz=TRUE)
dev.off()

# Fig 6.1 

pdf(file="C:/Country/Russia/Data/SEABYTE/RLMS/edreru/output/D_Fig 6.1.pdf")
par(mfrow=c(3,1))

# Gender
hist(s1b_98$FEMALE, main="Female = 1",col="dodgerblue",xlab="Female = 1 ",
     ylab="People")

# Nationality
hist(s1b_98$NON_RUSS, main="Non-Russian = 1",col="dodgerblue",xlab="Non-Russian = 1",ylab="People")

# Public Sector
hist(s1b_98$PUB_SEC,  main="Public Sector = 1",col="dodgerblue",xlab="Public Sector = 1",ylab="People")


dev.off()


# Regress LNWAGE ~ FEMALE + NONWHITE + UNION + EDUC + EXPER ---------------

reg.lnwage.by.educ.exper.female.nonr.pubs <- lm(lnwage.actual~EDUC+EXPER+NON_RUSS+FEMALE+PUB_SEC, data=s1b_98)
summary(reg.lnwage.by.educ.exper.female.nonr.pubs)

# RESET test
# No power terms needed p-values very high; low F, cannot 
# reject null of no higher orders needed
resettest(reg.lnwage.by.educ.exper.female.nonr.pubs, power=2, type="fitted")
resettest(reg.lnwage.by.educ.exper.female.nonr.pubs, power=2:3, type="fitted")

## In this case, needed, as F values ver high
resettest(reg.lnwage.by.educ.exper, power=2, type="fitted")
resettest(reg.lnwage.by.educ.exper, power=2:3, type="fitted")

reg3 <- lm(lnwage.actual~EDUC+I(EDUC^2)+EXPER+I(EXPER^2)+
             NON_RUSS+FEMALE+PUB_SEC+EDUC*EXPER+
             FEMALE*PUB_SEC+FEMALE*NON_RUSS+NON_RUSS*PUB_SEC, 
           data=s1b_98)
summary(reg3)

# Not much new to be learnt in this book ! 

