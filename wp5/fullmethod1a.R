# fullmethod1a.R

# adapted from rosstat1a.R by EM for new WP3

library(foreign)
library(plyr); library(dplyr)
library(gmodels)
library(lmtest)
library(sqldf)
library(XLConnectJars)
library(questionr)
library(labelled)
library(tidyr)
library(magrittr)
library(ggplot2)
library(data.table)
library(pbapply)
library(gridExtra)
library(nnet)
library(splitstackshape)
library(stringi)
library(stringr)
library(FinCal)
##########################################################################################################



## Now I get back data from Python
x <- round(c(23  , 24.025, 25.05 , 26.075, 27.1  , 28.125, 29.15 , 30.175,
             31.2  , 32.225, 33.25 , 34.275, 35.3  , 36.325, 37.35 , 38.375,
             39.4  , 40.425, 41.45 , 42.475, 43.5  , 44.525, 45.55 , 46.575,
             47.6  , 48.625, 49.65 , 50.675, 51.7  , 52.725, 53.75 , 54.775,
             55.8  , 56.825, 57.85 , 58.875, 59.9  , 60.925, 61.95 , 62.975,
             64))
y <- round(c(11946.        , 13745.58422046, 14250.08970563, 15744.54487672,
             17110.79639025, 18073.92063451, 18698.32492225, 19048.41656623,
             19188.60287918, 19183.29117387, 19096.88876304, 18993.80295946,
             18938.44107587, 18995.21042502, 19227.58413839, 19646.13052423,
             20181.17498803, 20756.34195502, 21295.25585042, 21721.54109943,
             21958.8221273 , 21933.77482112, 21639.39732883, 21132.93324622,
             20474.26213662, 19723.26356336, 18939.81708976, 18183.80227915,
             17515.09869485, 16993.5859002 , 16679.1434585 , 16631.6509331 ,
             16910.98788731, 17577.03388446, 18689.66848787, 20308.77126088,
             22494.22176681, 24894.46746954, 24949.10650281, 19359.64797414,
             4827.08114714
))

# We can't stop at 52, we will generate new numbers that shows a gentle
# decline from 17515 at age 52 to let's say 16000 at age 64

# first point (52, 17515)
# last point (64, 16000)

(x1 <- c(x[1:28]))
(y1 <- c(y[1:28]))

(x2 <- seq(from=52, to=64,length.out = 13))
(y2 <- round(seq(from=17515, to=16000,length.out = 13)))


x <- c(x1,x2)
y <- c(y1,y2)
plot(x,y,type='l')

# Our data is the 11th row in Tomsk oblast

df69_[11,]


# We assume that cash receipts are the tuition fee payments of students
# or private costs, fincome for fee income

(fincome=(df69_$cashReceipts_paidServices_2012+
    df69_$cashReceipts_paidServices_2013+
    df69_$cashReceipts_paidServices_2014+
    df69_$cashReceipts_paidServices_2015+
    df69_$cashReceipts_paidServices_2016+
    df69_$cashReceipts_paidServices_2017)/6)
# 11 is 25,140,717 rubles

# we use total income of establishment as the social cost

(tincome=(df69_$income_total_2012+df69_$income_total_2013+
            df69_$income_total_2014+df69_$income_total_2015+
            df69_$income_total_2016+
            df69_$income_total_2017)/6)

# 11 is 52543530 or 52,543,530 rubles  equiv to USD 750,000


# we use a heuristic - graduate_age 22 and over means 5 years training
# another heuristic - graduate age 21 or below means 4 years training

# here it is 22.9 hence 5 years

# We have about 100 graduates - we assume linear transitions

# So unit cost to society is about USD 1500 per year
# And private unit cost is about USD 750 per year

# For 5 years

# private costs
ifelse(df69_$graduates_number>0,fincome/(df69_$graduates_number*5),0)
# 51,837

# social costs
ifelse(df69_$graduates_number>0,tincome/(df69_$graduates_number*5),0)
# 108,337

# create a vector of costs for initial part of cash flow


# for private costs, based on fincome
(cprivate <- rep(-51837,5))  # for private
# social costs, based on tincome
(csocial <- rep(-108337,5))  #for society

# Then we generate returns input private
(rinpriv <- c(cprivate,y))

## And vector of cashflow social
(rinsoci <- c(csocial,y))

FinCal::irr(rinpriv)  # answer is 5.5% return
FinCal::irr(rinsoci)  # answer is 1.5% return 


