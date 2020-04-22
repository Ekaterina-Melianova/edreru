# IV_RLMS1a.R

library(hdm)
library(sqldf)
library(plyr); library(dplyr)
library(tidyr)

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
fem_ind_prop <- rgvars_2[, c('RoR_names', 'fem_ind_prop')] %>% drop_na()

# all IV candidates
ivs <- high_n %>%
  left_join(HSGPER, by = 'RoR_names') %>%
  left_join(EGE, by = 'RoR_names') %>%
  left_join(migrationrate, by = 'RoR_names') %>%
  left_join(women2menratio, by = 'RoR_names') %>%
  left_join(marriagerate, by = 'RoR_names') %>%
  left_join(fem_ind_prop, by = 'RoR_names')

#######################################################################################

# wd
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 
# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")
# Connecting with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")

### Main RLMS data
rlms <- readRDS('C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/df_mincer.rds')

# Filtering 2018 
rlms18 <- filter(rlms, YEAR == 2018)

##########################################################################
# Region
Region_rlms <- selectFromSQL(c("IDIND", "YEAR", "Region", "AGE")) %>% filter(YEAR == 2018)

# Merging
rlms18 <- rlms18 %>%
  left_join(Region_rlms[,c("IDIND", "YEAR", "REGION", "AGE")], by = c("IDIND", "YEAR"))

# Literacy 1897
Grig <- rio::import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.xlsx')
#Grig <- na.omit(Grig)
names(Grig)[5] <- 'REGION'

# Merging with rlms18
rlms18_Grig <- rlms18 %>%
  left_join(Grig[, c('Literacy_97', 'REGION', 'OKATO')], by = 'REGION')

# df
df <- rlms18_Grig

# Joining OKATO
df <- df %>%
  left_join(ivs, by = 'OKATO')

# Adding transformed vars
df$lnwage <- log(df$wage)
df$exper2 <- (df$exper)^2
df <- haven::zap_labels(df) 

# Selecting younger cohort
df_y <- df[df$AGE <= 36,]
df_o <- df[df$AGE >= 40 & df$AGE <= 51,]

#######################################################  Lm

# Females
lm.fem.all.y <- lm(log(wage) ~ edu_yrs + exper + I(exper^2), data = df_y[df_y$female == 1, ])
summary(lm.fem.all.y)

lm.fem.all.o <- lm(log(wage) ~ edu_yrs + exper + I(exper^2), data = df_o[df_o$female == 1, ])
summary(lm.fem.all.o)

# Males
lm.fem.all.y <- lm(log(wage) ~ edu_yrs + exper + I(exper^2), data = df_y[df_y$female == 0, ])
summary(lm.fem.all.y)

lm.fem.all.o <- lm(log(wage) ~ edu_yrs + exper + I(exper^2), data = df_o[df_o$female == 0, ])
summary(lm.fem.all.o)

####################################################### ivmodel
# Females
# Young
ivmodel.fem.all.y <- ivmodel(Y = df_y[df_y$female == 1, 'lnwage'],
                             D = df_y[df_y$female == 1, 'edu_yrs'],
                             Z = df_y[df_y$female == 1, c('high_n', 'HSGPER', 's1z',
                                                          'migrationrate',
                                                          'women2menratio',
                                                          'marriagerate',
                                                          'fem_ind_prop')],
                             X = df_y[df_y$female == 1, c('exper', 'exper2')])
ivmodel.fem.all.y

# Old
ivmodel.fem.all.o <- ivmodel(Y = df_o[df_o$female == 1, 'lnwage'],
                             D = df_o[df_o$female == 1, 'edu_yrs'],
                             Z = df_o[df_o$female == 1, c('high_n', 'HSGPER', 's1z',
                                                          'migrationrate',
                                                          'women2menratio',
                                                          'marriagerate',
                                                          'fem_ind_prop')],
                             X = df_o[df_o$female == 1, c('exper', 'exper2')])
ivmodel.fem.all.o

# Males
# Young
ivmodel.male.all.y <- ivmodel(Y = df_y[df_y$female == 0, 'lnwage'],
                             D = df_y[df_y$female == 0, 'edu_yrs'],
                             Z = df_y[df_y$female == 0, c('high_n', 'HSGPER', 's1z',
                                                          'migrationrate',
                                                          'women2menratio',
                                                          'marriagerate',
                                                          'fem_ind_prop')],
                             X = df_y[df_y$female == 0, c('exper', 'exper2')])
ivmodel.male.all.y

# Old
ivmodel.male.all.o <- ivmodel(Y = df_o[df_o$female == 0, 'lnwage'],
                             D = df_o[df_o$female == 0, 'edu_yrs'],
                             Z = df_o[df_o$female == 0, c('high_n', 'HSGPER', 's1z',
                                                          'migrationrate',
                                                          'women2menratio',
                                                          'marriagerate',
                                                          'fem_ind_prop')],
                             X = df_o[df_o$female == 0, c('exper', 'exper2')])
ivmodel.male.all.o

# Aggregating all the parameters
Sample <- rep(rep(c('fem.all', 'males'), each = 4),2)
Estimator <-  rep(c( 'OLS', 'TSLS', 'Fuller', 'LIML'), 4)
Cohort <- rep(c('younger', 'older'), each = 8)
ivmodel.coefs <- cbind(Cohort, Sample, Estimator,
                       round(rbind.data.frame(as.data.frame(ivmodel.fem.all.y$kClass),
                                        as.data.frame(ivmodel.fem.all.y$Fuller)[1:6], 
                                        as.data.frame(ivmodel.fem.all.y$LIML)[1:6],
                                        as.data.frame(ivmodel.male.all.y$kClass),
                                        as.data.frame(ivmodel.male.all.y$Fuller)[1:6], 
                                        as.data.frame(ivmodel.male.all.y$LIML)[1:6],
                                        as.data.frame(ivmodel.fem.all.o$kClass),
                                        as.data.frame(ivmodel.fem.all.o$Fuller)[1:6], 
                                        as.data.frame(ivmodel.fem.all.o$LIML)[1:6],
                                        as.data.frame(ivmodel.male.all.o$kClass),
                                        as.data.frame(ivmodel.male.all.o$Fuller)[1:6], 
                                        as.data.frame(ivmodel.male.all.o$LIML)[1:6]), 3))
colnames(ivmodel.coefs)[4:ncol(ivmodel.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'ci2.5', 'ci97.5')
ivmodel.coefs

#######################################################  post-lasso
# Females all
postLasso.fem.all.y <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                         high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                         marriagerate + fem_ind_prop,
                                       data = df_y[df_y$female == 1,])
as.data.frame(postLasso.fem.all.y$selected)
as.numeric(summary(postLasso.fem.all.y))

postLasso.fem.all.o <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                         high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                         marriagerate + fem_ind_prop,
                                       data = df_o[df_o$female == 1,])
as.data.frame(postLasso.fem.all.o$selected)
as.numeric(summary(postLasso.fem.all.o))

# Males
postLasso.male.all.y <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                          high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                          marriagerate + fem_ind_prop,
                                        data = df_y[df_y$female == 0,])
as.data.frame(postLasso.male.all.y$selected)
summary(postLasso.male.all.y)

postLasso.male.all.o <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                          high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                          marriagerate + fem_ind_prop,
                                        data = df_o[df_o$female == 0,])
as.data.frame(postLasso.male.all.o$selected)
summary(postLasso.male.all.o)

# Aggregating all the parameters from post-Lasso
Sample <- c('fem.all', 'males')
Cohort <- rep(c('younger', 'older'), each = 2)
postLasso.coefs <- cbind(Cohort, Sample, 
                         round(rbind.data.frame(as.numeric(summary(postLasso.fem.all.y)),
                                          as.numeric(summary(postLasso.male.all.y)),
                                          as.numeric(summary(postLasso.fem.all.o)),
                                          as.numeric(summary(postLasso.male.all.o))), 3))
colnames(postLasso.coefs)[3:ncol(postLasso.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue')
postLasso.coefs
