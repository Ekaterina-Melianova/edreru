# IVLasso2a.R

library(dplyr)
library(tidyr)
library(hdm)
library(lme4)
library(ivmodel)
library(naivereg)
library(rio)
library(npsr)

#install.packages('readstata13', dependencies = T)
########################################### Data  ################################################

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

###################################################################################################
######################################### Analysis ################################################
###################################################################################################

# (i) Examine correlation with Edu_years one by one

######################## Females all
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'high_n'])$estimate
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 's1z'])$estimate
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'migrationrate'])$estimate
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'women2menratio'])$estimate
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 1, 'edu_yrs'], df[df$H01_01 == 1, 'fem_ind_prop'])$estimate

######################## Females married
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'high_n'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 's1z'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'migrationrate'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'women2menratio'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 1, 'fem_ind_prop'])$estimate

######################## Females all
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'high_n'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 's1z'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'migrationrate'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'women2menratio'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
         df[df$H01_01 == 1 & df$married == 0, 'fem_ind_prop'])$estimate

######################## Males all
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'high_n'])$estimate
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 's1z'])$estimate
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'migrationrate'])$estimate
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'women2menratio'])$estimate
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'marriagerate'])$estimate
cor.test(df[df$H01_01 == 2, 'edu_yrs'], df[df$H01_01 == 2, 'fem_ind_prop'])$estimate

###########################  (ii) Use Kang et al ivmodel for running ols, TSLS standard with all IVs;

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

# Females married
ivmodel.fem.married <- ivmodel(Y = df[df$H01_01 == 1 & df$married == 1, 'lnwage'],
                           D = df[df$H01_01 == 1 & df$married == 1, 'edu_yrs'],
                           Z = df[df$H01_01 == 1 & df$married == 1, c('high_n', 'HSGPER', 's1z',
                                      'migrationrate',
                                      'women2menratio',
                                      'marriagerate',
                                      'fem_ind_prop')],
                           X = df[df$H01_01 == 1 & df$married == 1, c('exper', 'exper2')])
ivmodel.fem.married

# Females single
ivmodel.fem.single <- ivmodel(Y = df[df$H01_01 == 1 & df$married == 0, 'lnwage'],
                           D = df[df$H01_01 == 1 & df$married == 0, 'edu_yrs'],
                           Z = df[df$H01_01 == 1 & df$married == 0, c('high_n', 'HSGPER', 's1z',
                                      'migrationrate',
                                      'women2menratio',
                                      'marriagerate',
                                      'fem_ind_prop')],
                           X = df[df$H01_01 == 1 & df$married == 0, c('exper', 'exper2')])
ivmodel.fem.single

# Males
ivmodel.male.all <- ivmodel(Y = df[df$H01_01 == 2, 'lnwage'],
                           D = df[df$H01_01 == 2, 'edu_yrs'],
                           Z = df[df$H01_01 == 2, c('high_n', 'HSGPER', 's1z',
                                      'migrationrate',
                                      'women2menratio',
                                      'marriagerate',
                                      'fem_ind_prop')],
                           X = df[df$H01_01 == 2, c('exper', 'exper2')])
ivmodel.male.all

# Aggregating all the parameters
Sample <- rep(c('fem.all', 'fem.married', 'fem.single', 'males'), each = 4)
Estimator <-  rep(c('TSLS', 'OLS', 'Fuller', 'LIML'), 4)
ivmodel.coefs <- cbind(Sample, Estimator,
                       rbind.data.frame(as.data.frame(ivmodel.fem.all$kClass),
                                  as.data.frame(ivmodel.fem.all$Fuller)[1:6], 
                                  as.data.frame(ivmodel.fem.all$LIML)[1:6],
                                  as.data.frame(ivmodel.fem.married$kClass),
                                  as.data.frame(ivmodel.fem.married$Fuller)[1:6], 
                                  as.data.frame(ivmodel.fem.married$LIML)[1:6],
                                  as.data.frame(ivmodel.fem.single$kClass),
                                  as.data.frame(ivmodel.fem.single$Fuller)[1:6], 
                                  as.data.frame(ivmodel.fem.single$LIML)[1:6],
                                  as.data.frame(ivmodel.male.all$kClass),
                                  as.data.frame(ivmodel.male.all$Fuller)[1:6], 
                                  as.data.frame(ivmodel.male.all$LIML)[1:6]))
colnames(ivmodel.coefs)[3:ncol(ivmodel.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'ci2.5', 'ci97.5')
ivmodel.coefs


########################### (iii) Use HDM and Post Lasso and look at results

# Estimating post-lasso
# Females all
postLasso.fem.all <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                   high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                   marriagerate + fem_ind_prop,
                                 data = df[df$H01_01 == 1,])
as.data.frame(postLasso.fem.all$selected)
as.numeric(summary(postLasso.fem.all))

# Females married
postLasso.fem.married <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                       high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                       marriagerate + fem_ind_prop,
                                     data = df[df$H01_01 == 1 & df$married == 1,])
as.data.frame(postLasso.fem.married$selected)
summary(postLasso.fem.married)

# Females single
postLasso.fem.single <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                       high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                       marriagerate + fem_ind_prop,
                                     data = df[df$H01_01 == 1 & df$married == 0,])
as.data.frame(postLasso.fem.single$selected)
summary(postLasso.fem.single)

# Males
postLasso.male.all <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                       high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                       marriagerate + fem_ind_prop,
                                     data = df[df$H01_01 == 2,])
as.data.frame(postLasso.male.all$selected)
summary(postLasso.male.all)

# Aggregating all the parameters from post-Lasso
Sample <- c('fem.all', 'fem.married', 'fem.single', 'males')
postLasso.coefs <- cbind(Sample, 
                          rbind.data.frame(as.numeric(summary(postLasso.fem.all)),
                                        as.numeric(summary(postLasso.fem.married)),
                                        as.numeric(summary(postLasso.fem.single)),
                                        as.numeric(summary(postLasso.male.all))))
colnames(postLasso.coefs)[2:ncol(postLasso.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue')
postLasso.coefs

########################### (iv) Use naïvereg Fan and Zhong for alternative
# !!! Time consuming

df <- na.omit(df)

# Female all
#naivereg.fem.all <- naivereg(y = df[df$H01_01 == 1, 'lnwage'],
#                    x = df[df$H01_01 == 1, c('edu_yrs', 'exper', 'exper2')],
#                    z = df[df$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
#                                 'migrationrate',
#                                 'women2menratio',
#                                 'marriagerate',
 #                                 'fem_ind_prop')],
#                    criterion = 'BIC')
#naivereg.fem.all$coefficients

# Female married
#naivereg.fem.married <- naivereg(y = df[df$H01_01 == 1, 'lnwage'],
#                             x = df[df$H01_01 == 1, c('edu_yrs', 'exper', 'exper2')],
#                            z = df[df$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
#                                                     'migrationrate',
#                                                      'women2menratio',
#                                                      'marriagerate',
#                                                      'fem_ind_prop')],
#                             criterion = 'BIC')
#naivereg.fem.married$coefficients

# Female signle
#naivereg.fem.single <- naivereg(y = df[df$H01_01 == 1, 'lnwage'],
#                             x = df[df$H01_01 == 1, c('edu_yrs', 'exper', 'exper2')],
#                             z = df[df$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
#                                                      'migrationrate',
#                                                      'women2menratio',
#                                                      'marriagerate',
#                                                      'fem_ind_prop')],
#                             criterion = 'BIC')
#naivereg.fem.single$coefficients

# Male
#naivereg.male.all <- naivereg(y = df[df$H01_01 == 1, 'lnwage'],
#                             x = df[df$H01_01 == 1, c('edu_yrs', 'exper', 'exper2')],
#                             z = df[df$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
#                                                     'migrationrate',
#                                                      'women2menratio',
#                                                     'marriagerate',
#                                                      'fem_ind_prop')],
#                             criterion = 'BIC')
#naivereg.male.all$coefficients

##########################  (v) Possibly some aspects of Sharma and npsr package.

# !!! Veeery time consuming

#nps.fem.all <- nps.test(data.frame(y = df[df$H01_01 == 1, 'lnwage'],
#                    x = df[df$H01_01 == 1, c('edu_yrs')],
#                   z = df[df$H01_01 == 1, c('high_n')]), 2, 2, 2, 3, 3)




