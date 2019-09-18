# diebold1a.R
# A file to replicate from RLMS 1998 2008 2017, what Diebold does with CPS 1995 2004 and 2012
# Econometric Data Science Text from Diebold


options(scipen=999) # to supress scientific notation
Sys.setlocale("LC_ALL", "russian")

# Data: joined base
library(foreign)
library(dplyr)

memory.limit(1e10)
dat_joined <- foreign::read.spss(file="C:/Country/Russia/Data/SEABYTE/RLMS/rawdata/Joined_database/USER_RLMS-HSE_IND_1994_2017_v2.sav",
                                 use.value.labels = F,
                                 use.missings=TRUE,
                                 to.data.frame = TRUE)


s1a_98 <- dat_joined[which(dat_joined$YEAR==1998 & dat_joined$J13.2>0),c("ID_I","ID_H","J13.2","EDUC","J5A","J5B","H7.2")]

hist(s1a_98$J13.2,breaks=50,col="turquoise")
hist(log(s1a_98$J13.2),breaks=30,col="blue")


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

# Experience has to be constructed
# There is year and month information, 

# Let's calculate number of months of interview date, we know year is 1998
s1a_98$IMTHS <- (1997*12)+s1a_98$H7.2
# Let's calculate number of months of start date of 1st job - we will need to revise later for > 1 job
s1a_98$SWORK <- (s1a_98$J5A*12)+(s1a_98$J5B)

s1a_98$EXPER <- (s1a_98$IMTHS-s1a_98$SWORK)/12


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


