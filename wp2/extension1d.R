# extension1d.R
options(scipen=999) # to supress scientific notation
# Arrazola and De Havia / Weber et al 

library(plyr); library(dplyr)
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
library(psych)
library(stringi)
library(sjPlot)
library(sjmisc)
library(segregation)
library(reshape2)
library(arules) 

# Some functions
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Specify the default working directory for this script
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2")

# Adjusting to prices in 2018
mt19 <- rio::import("mt19_table.xlsx")

df_mincer <- readRDS("df_mincer.rds")
df_mincer2 <- left_join(df_mincer,mt19,by = c("occup" = "isco_08"))

df_mincer2$NRAIM=df_mincer2$NRA+df_mincer2$NRI+df_mincer2$NRM
df_mincer2$RCM=df_mincer2$RC+df_mincer2$RM

#################################### For 2018 ##########################################

df_18 <- df_mincer2 %>% filter(YEAR==2018) 

df_18f <- df_18 %>% select(IDIND, wage, edu_4,exper,female,edu_yrs)  %>% filter(female==1) %>%
               mutate(lnwage=log(wage), d_voc=ifelse(edu_4=="Vocational",1,0),
                      d_uni=ifelse(edu_4=="Higher",1,0), tlabor0=64-edu_yrs)  # using 64 for now, will need to adjust
df_18f <- haven::zap_labels(df_18f) # Else rio gives error message


df_18m <- df_18 %>% select(IDIND, wage, edu_4,exper,female,edu_yrs)  %>% filter(female==0) %>%
  mutate(lnwage=log(wage),d_voc=ifelse(edu_4=="Vocational",1,0),
         d_uni=ifelse(edu_4=="Higher",1,0), tlabor0=64-edu_yrs)  # using 64 for now, will need to adjusttlabor0=64-edu_yrs) # using 64 for now, will need to adjust
df_18m <- haven::zap_labels(df_18m) # Else rio gives error message

rio::export(df_18f, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_18f.dta",format="stata")
rio::export(df_18m, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_18m.dta",format="stata")



# Try for model with covariates - just placeholder
df_18fz <- df_18 %>% select(IDIND, wage, edu_4,exper,female,edu_yrs,RTI)  %>% filter(female==1) %>%
  mutate(lnwage=log(wage), d_voc=ifelse(edu_4=="Vocational",1,0),
         d_uni=ifelse(edu_4=="Higher",1,0), tlabor0=64-edu_yrs)  # using 64 for now, will need to adjust
df_18fz <- haven::zap_labels(df_18fz) # Else rio gives error message

rio::export(df_18fz, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_18fz.dta",format="stata")

#################################### For 6 years ##########################################

# Selecting 6 years
df_6yrs <- df_mincer2 %>% filter(YEAR == 1994|YEAR == 1998|YEAR == 2003|
                                   YEAR == 2006|YEAR == 2012|YEAR == 2018) 

# Data for females
df_6yrsf <- df_6yrs %>% select(YEAR, IDIND, wage, edu_4, exper, female, edu_yrs) %>%
  filter(female == 1) %>%
  mutate(lnwage = log(wage), d_voc = ifelse(edu_4 == "Vocational",1,0),
         d_uni = ifelse(edu_4 == "Higher", 1, 0), tlabor0 = 64 - edu_yrs)  # using 64 for now, will need to adjust
df_6yrsf <- haven::zap_labels(df_6yrsf) # Else rio gives error message

# Data for males
df_6yrsm <- df_6yrs %>% select(YEAR, IDIND, wage, edu_4, exper, female, edu_yrs) %>%
  filter(female == 0) %>%
  mutate(lnwage = log(wage), d_voc = ifelse(edu_4 == "Vocational", 1, 0),
         d_uni = ifelse(edu_4 == "Higher", 1, 0), tlabor0 = 64 - edu_yrs)  # using 64 for now, will need to adjusttlabor0=64-edu_yrs) # using 64 for now, will need to adjust
df_6yrsm <- haven::zap_labels(df_6yrsm) # Else rio gives error message

# Data for all
df_6yrsall <- df_6yrs %>% select(YEAR, IDIND, wage, edu_4, exper, female, edu_yrs) %>%
  mutate(lnwage = log(wage), d_voc = ifelse(edu_4 == "Vocational", 1, 0),
         d_uni = ifelse(edu_4 == "Higher", 1, 0), tlabor0 = 64 - edu_yrs)  # using 64 for now, will need to adjusttlabor0=64-edu_yrs) # using 64 for now, will need to adjust
df_6yrsall <- haven::zap_labels(df_6yrsall) # Else rio gives error message

# Exporting
rio::export(df_6yrsf, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_6yrsf.dta",format="stata")
rio::export(df_6yrsm, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_6yrsm.dta",format="stata")
rio::export(df_6yrsall, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_6yrsall.dta",format="stata")

