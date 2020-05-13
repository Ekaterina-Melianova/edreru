# IVLasso2aSP.R

library(dplyr)
library(tidyr)
library(hdm)
library(lme4)
library(ivmodel)
library(naivereg)
library(rio)
library(npsr)

Sys.setlocale("LC_CTYPE", "russian")
# load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/EGE1d.rdata')
# load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/EGE1e.rdata')
# load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/PISA01.rdata')
load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/qq1.rdata')
RoR_1990_2015_rub <- readstata13::read.dta13('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/RoR_1990_2015_rub.dta')
rgvars <- import("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/rgvars.xlsx")
rgvars_2 <- import("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/rgvars_2.xlsx")

# Taking last year
RoR_15 <- RoR_1990_2015_rub %>% filter(year == 2015)
names(RoR_15)[which(names(RoR_15) == 'region')] <- 'RoR_names'

###### Potential IV

#### QUANTITY
# high_n : number of HE institutions
high_n <- RoR_15[, c('RoR_names', 'high_n')]

# HSGPER: High School graduate students per school
HSGPER <- qq1[, c('region', 'HSGPER')]
names(HSGPER)[1] <- 'RoR_names'

### QUALITY
# EGE
EGE <- rgvars[, c('RoR_names', 's1z', 'OKATO')] %>% drop_na()

### MIGRATION
# migrationrate: Net migration rate (from ROR Stata file)
migrationrate <- RoR_15[, c('RoR_names', 'migrationrate')]

### WOMEN
# women2menratio
women2menratio <- RoR_15[, c('RoR_names', 'women2menratio')]

# marriagerate
marriagerate <- RoR_15[, c('RoR_names', 'marriagerate')]

# fem_industry share
# fem_ind_prop <- rgvars_2[, c('RoR_names', 'fem_ind_prop', 'OKATO')] %>% drop_na()

# all IV candidates
ivs <- high_n %>%
  left_join(HSGPER, by = 'RoR_names') %>%
  left_join(EGE, by = 'RoR_names') %>%
  left_join(migrationrate, by = 'RoR_names') %>%
  left_join(women2menratio, by = 'RoR_names') %>%
  left_join(marriagerate, by = 'RoR_names')

# Adding them to the Rosstat df
###### Rosstat main dataset

wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2'
setwd(wd)
Rosstat18 <- readRDS('Rosstat18.rds')
names(Rosstat18)[1] <- 'OKATO'

# Merging
df <- Rosstat18 %>%
  left_join(ivs, by = 'OKATO')

# Marital status
df$married <- ifelse(df$H01_04 == 1 | df$H01_04 == 2, 1, 0)

# Adding transformed vars
df$lnwage <- log(df$wage)
df$exper2 <- (df$exper)^2

# Selecting younger cohort
df <- df[df$H01_02 <= 35,]


# Analysis

# (i) Examine correlation with Edu_years one by one



######################## Females all
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'high_n'])$estimate
##       cor 
## 0.1916178
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 's1z'])$estimate
##       cor 
## 0.1052094
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'migrationrate'])$estimate
##       cor 
## 0.1302901
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'women2menratio'])$estimate
##         cor 
## 0.008004981
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'marriagerate'])$estimate
##        cor 
## 0.02495572
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'marriagerate'])$estimate
##        cor 
## 0.02495572
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'fem_ind_prop'])$estimate
##        cor 
## -0.1168035
######################## Females married
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'high_n'])$estimate
##     cor 
## 0.18683
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 's1z'])$estimate
##       cor 
## 0.1102623
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'migrationrate'])$estimate
##       cor 
## 0.1253053
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'women2menratio'])$estimate
##         cor 
## 0.005836721
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'marriagerate'])$estimate
##        cor 
## 0.03051481
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'marriagerate'])$estimate
##        cor 
## 0.03051481
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'fem_ind_prop'])$estimate
##        cor 
## -0.1138125
######################## Females all
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'high_n'])$estimate
##       cor 
## 0.2025128
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 's1z'])$estimate
##        cor 
## 0.09725601
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'migrationrate'])$estimate
##       cor 
## 0.1433493
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'women2menratio'])$estimate
##        cor 
## 0.01424987
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'marriagerate'])$estimate
##        cor 
## 0.01249563
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'marriagerate'])$estimate
##        cor 
## 0.01249563
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'fem_ind_prop'])$estimate
##        cor 
## -0.1302252
######################## Males all
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'high_n'])$estimate
##       cor 
## 0.1427025
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 's1z'])$estimate
##       cor 
## 0.1061641
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'migrationrate'])$estimate
##        cor 
## 0.09468768
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'women2menratio'])$estimate
##         cor 
## 0.002323698
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'marriagerate'])$estimate
##        cor 
## 0.05076977
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'marriagerate'])$estimate
##        cor 
## 0.05076977
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'fem_ind_prop'])$estimate
##         cor 
## -0.09797761

#(ii) Use Kang et al ivmodel for running ols, TSLS standard with all IVs

# Females all
ivmodel.fem.all <- ivmodel(Y = df[df$H01_01 == 1, 'lnwage'],
                           D = df[df$H01_01 == 1, 'edu_yrs'],
                           Z = df[df$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
                                                    'migrationrate',
                                                    'women2menratio',
                                                    'marriagerate',
                                                    'fem_ind_prop')],
                           X = df[df$H01_01 == 1, c('exper', 'exper2')])
ivmodel.fem.all


