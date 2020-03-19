# IV_Rosstat.R

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
df_y <- df[df$H01_02 <= 36,]
df_o <- df[df$H01_02 >= 40 & df$H01_02 <= 51,]

###################################################################################################
######################################### Analysis ################################################
###################################################################################################

# (i) Examine correlation with Edu_years one by one

######################## Females all
Z <- c('high_n', 's1z', 'migrationrate', 'women2menratio', 'marriagerate', 'fem_ind_prop')

cor_fem_all_yonger <- c()
cor_fem_all_older <- c()
cor_fem_mar_younger <- c()
cor_fem_mar_older <- c()
cor_fem_sngl_younger <- c()
cor_fem_sngl_older <- c()
cor_male_all_yonger <- c()
cor_male_all_older <- c()

for (i in 1:length(Z)){
  # Females all
  cor_fem_all_yonger <- c(cor_fem_all_yonger, 
                          eval(parse(text = paste0("cor.test(df_y[df_y$H01_01 == 1, 'edu_yrs'], df_y[df_y$H01_01 == 1, ",
                                                 paste(" '", Z[i], "' ", sep = ''), "])$estimate"))))
  cor_fem_all_older <- c(cor_fem_all_older,
                          eval(parse(text = paste0('cor.test(df_o[df_o$H01_01 == 1, "edu_yrs"], df_o[df_o$H01_01 == 1, ',
                                                 paste(" '", Z[i], "' ", sep = ''), '])$estimate'))))
  # Females married
  cor_fem_mar_younger <- c(cor_fem_mar_younger,
                           eval(parse(text = paste0('cor.test(df_y[df_y$H01_01 == 1 & df_y$married == 1, "edu_yrs"], df_y[df_y$H01_01 == 1 & df_y$married == 1, ',
                                                  paste(" '", Z[i], "' ", sep = ''), '])$estimate'))))
  cor_fem_mar_older <- c(cor_fem_mar_older,
                         eval(parse(text = paste0('cor.test(df_o[df_o$H01_01 == 1 & df_o$married == 1, "edu_yrs"], df_o[df_o$H01_01 == 1 & df_o$married == 1, ',
                                                  paste(" '", Z[i], "' ", sep = ''), '])$estimate'))))
  
  # Females single
  cor_fem_sngl_younger <- c(cor_fem_sngl_younger,
                            eval(parse(text = paste0('cor.test(df_y[df_y$H01_01 == 1 & df_y$married == 0, "edu_yrs"], df_y[df_y$H01_01 == 1 & df_y$married == 0, ',
                                                  paste(" '", Z[i], "' ", sep = ''), '])$estimate'))))
  cor_fem_sngl_older <- c(cor_fem_sngl_older,
                          eval(parse(text = paste0('cor.test(df_y[df_y$H01_01 == 1 & df_y$married == 0, "edu_yrs"], df_y[df_y$H01_01 == 1 & df_y$married == 0, ',
                                                paste(" '", Z[i], "' ", sep = ''), '])$estimate'))))
  
  # Males
  cor_male_all_yonger <- c(cor_male_all_yonger,
                           eval(parse(text = paste0("cor.test(df_y[df_y$H01_01 == 2, 'edu_yrs'], df_y[df_y$H01_01 == 2, ",
                                                 paste(" '", Z[i], "' ", sep = ''), "])$estimate"))))
  cor_male_all_older <- c(cor_male_all_older,
                          eval(parse(text = paste0('cor.test(df_o[df_o$H01_01 == 2, "edu_yrs"], df_o[df_o$H01_01 == 2, ',
                                                 paste(" '", Z[i], "' ", sep = ''), '])$estimate'))))
}

cor_df <- cbind.data.frame(Z, round(cbind.data.frame(cor_fem_all_yonger,
                           cor_fem_all_older,
                           cor_fem_mar_younger,
                           cor_fem_mar_older,
                           cor_fem_sngl_younger,
                           cor_fem_sngl_older,
                           cor_male_all_yonger,
                           cor_male_all_older), 2))

###########################  (ii) Use Kang et al ivmodel for running ols, TSLS standard with all IVs;

# Females all
# Younger
ivmodel.fem.all.y <- ivmodel(Y = df_y[df_y$H01_01 == 1, 'lnwage'],
                       D = df_y[df_y$H01_01 == 1, 'edu_yrs'],
                       Z = df_y[df_y$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
                                  'migrationrate',
                                  'women2menratio',
                                  'marriagerate',
                                  'fem_ind_prop')],
                       X = df_y[df_y$H01_01 == 1, c('exper', 'exper2')])
ivmodel.fem.all.y

# Older
ivmodel.fem.all.o <- ivmodel(Y = df_o[df_o$H01_01 == 1, 'lnwage'],
                             D = df_o[df_o$H01_01 == 1, 'edu_yrs'],
                             Z = df_o[df_o$H01_01 == 1, c('high_n', 'HSGPER', 's1z',
                                                      'migrationrate',
                                                      'women2menratio',
                                                      'marriagerate',
                                                      'fem_ind_prop')],
                             X = df_o[df_o$H01_01 == 1, c('exper', 'exper2')])
ivmodel.fem.all.o

# Females married
# Younger
ivmodel.fem.married.y <- ivmodel(Y = df_y[df_y$H01_01 == 1 & df_y$married == 1, 'lnwage'],
                           D = df_y[df_y$H01_01 == 1 & df_y$married == 1, 'edu_yrs'],
                           Z = df_y[df_y$H01_01 == 1 & df_y$married == 1, c('high_n', 'HSGPER', 's1z',
                                      'migrationrate',
                                      'women2menratio',
                                      'marriagerate',
                                      'fem_ind_prop')],
                           X = df_y[df_y$H01_01 == 1 & df_y$married == 1, c('exper', 'exper2')])
ivmodel.fem.married.y

# Older
ivmodel.fem.married.o <- ivmodel(Y = df_o[df_o$H01_01 == 1 & df_o$married == 1, 'lnwage'],
                                 D = df_o[df_o$H01_01 == 1 & df_o$married == 1, 'edu_yrs'],
                                 Z = df_o[df_o$H01_01 == 1 & df_o$married == 1, c('high_n', 'HSGPER', 's1z',
                                                                            'migrationrate',
                                                                            'women2menratio',
                                                                            'marriagerate',
                                                                            'fem_ind_prop')],
                                 X = df_o[df_o$H01_01 == 1 & df_o$married == 1, c('exper', 'exper2')])
ivmodel.fem.married.o

# Females single
# Younger
ivmodel.fem.single.y <- ivmodel(Y = df_y[df_y$H01_01 == 1 & df_y$married == 0, 'lnwage'],
                           D = df_y[df_y$H01_01 == 1 & df_y$married == 0, 'edu_yrs'],
                           Z = df_y[df_y$H01_01 == 1 & df_y$married == 0, c('high_n', 'HSGPER', 's1z',
                                      'migrationrate',
                                      'women2menratio',
                                      'marriagerate',
                                      'fem_ind_prop')],
                           X = df_y[df_y$H01_01 == 1 & df_y$married == 0, c('exper', 'exper2')])
ivmodel.fem.single.y

# Older
ivmodel.fem.single.o <- ivmodel(Y = df_o[df_o$H01_01 == 1 & df_o$married == 0, 'lnwage'],
                                D = df_o[df_o$H01_01 == 1 & df_o$married == 0, 'edu_yrs'],
                                Z = df_o[df_o$H01_01 == 1 & df_o$married == 0, c('high_n', 'HSGPER', 's1z',
                                                                           'migrationrate',
                                                                           'women2menratio',
                                                                           'marriagerate',
                                                                           'fem_ind_prop')],
                                X = df_o[df_o$H01_01 == 1 & df_o$married == 0, c('exper', 'exper2')])
ivmodel.fem.single.o

# Males
# Younger
ivmodel.male.all.y <- ivmodel(Y = df_y[df_y$H01_01 == 2, 'lnwage'],
                           D = df_y[df_y$H01_01 == 2, 'edu_yrs'],
                           Z = df_y[df_y$H01_01 == 2, c('high_n', 'HSGPER', 's1z',
                                      'migrationrate',
                                      'women2menratio',
                                      'marriagerate',
                                      'fem_ind_prop')],
                           X = df_y[df_y$H01_01 == 2, c('exper', 'exper2')])
ivmodel.male.all.y

# Older
ivmodel.male.all.o <- ivmodel(Y = df_o[df_o$H01_01 == 2, 'lnwage'],
                              D = df_o[df_o$H01_01 == 2, 'edu_yrs'],
                              Z = df_o[df_o$H01_01 == 2, c('high_n', 'HSGPER', 's1z',
                                                       'migrationrate',
                                                       'women2menratio',
                                                       'marriagerate',
                                                       'fem_ind_prop')],
                              X = df_o[df_o$H01_01 == 2, c('exper', 'exper2')])
ivmodel.male.all.o

# Aggregating all the parameters
Sample <- rep(rep(c('fem.all', 'fem.married', 'fem.single', 'males'), each = 4),2)
Estimator <-  rep(c( 'OLS', 'TSLS', 'Fuller', 'LIML'), 8)
Cohort <- rep(c('younger', 'older'), each = 16)
ivmodel.coefs <- cbind(Cohort, Sample, Estimator,
                       rbind.data.frame(as.data.frame(ivmodel.fem.all.y$kClass),
                                  as.data.frame(ivmodel.fem.all.y$Fuller)[1:6], 
                                  as.data.frame(ivmodel.fem.all.y$LIML)[1:6],
                                  as.data.frame(ivmodel.fem.married.y$kClass),
                                  as.data.frame(ivmodel.fem.married.y$Fuller)[1:6], 
                                  as.data.frame(ivmodel.fem.married.y$LIML)[1:6],
                                  as.data.frame(ivmodel.fem.single.y$kClass),
                                  as.data.frame(ivmodel.fem.single.y$Fuller)[1:6], 
                                  as.data.frame(ivmodel.fem.single.y$LIML)[1:6],
                                  as.data.frame(ivmodel.male.all.y$kClass),
                                  as.data.frame(ivmodel.male.all.y$Fuller)[1:6], 
                                  as.data.frame(ivmodel.male.all.y$LIML)[1:6],
                       as.data.frame(ivmodel.fem.all.o$kClass),
                       as.data.frame(ivmodel.fem.all.o$Fuller)[1:6], 
                       as.data.frame(ivmodel.fem.all.o$LIML)[1:6],
                       as.data.frame(ivmodel.fem.married.o$kClass),
                       as.data.frame(ivmodel.fem.married.o$Fuller)[1:6], 
                       as.data.frame(ivmodel.fem.married.o$LIML)[1:6],
                       as.data.frame(ivmodel.fem.single.o$kClass),
                       as.data.frame(ivmodel.fem.single.o$Fuller)[1:6], 
                       as.data.frame(ivmodel.fem.single.o$LIML)[1:6],
                       as.data.frame(ivmodel.male.all.o$kClass),
                       as.data.frame(ivmodel.male.all.o$Fuller)[1:6], 
                       as.data.frame(ivmodel.male.all.o$LIML)[1:6]))
colnames(ivmodel.coefs)[4:ncol(ivmodel.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'ci2.5', 'ci97.5')
ivmodel.coefs

########################################## LM
# Females all
# Younger
lm.fem.all.y <- lm(lnwage ~ edu_yrs + exper + I(exper^2), data = df_y[df_y$H01_01 == 1, ])
summary(lm.fem.all.y) # coinsides with ivmodel

# Older
lm.fem.all.o <- lm(lnwage ~ edu_yrs + exper + exper2, data = df_o[df_o$H01_01 == 1, ])
summary(lm.fem.all.o) # coinsides with ivmodel, so should the rest

########################################## IV regressions with ivreg
# Females
library(AER)

# Younger
ivreg.fem.all.y <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2)|
                 exper + I(exper^2) + high_n + HSGPER + s1z +
                 migrationrate + women2menratio + marriagerate +
                 fem_ind_prop,
                 data = df_y[df_y$H01_01 == 1,])
summary(ivreg.fem.all.y, vcov = sandwich, diagnostics = T) # the same as ivmodel

# Older
ivreg.fem.all.o <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2)|
                           exper + I(exper^2) + high_n + HSGPER + s1z +
                           migrationrate + women2menratio + marriagerate +
                           fem_ind_prop,
                         data = df_o[df_o$H01_01 == 1,])
summary(ivreg.fem.all.o, vcov = sandwich, diagnostics = T) # the same as ivmodel

# So the problem is not in ivmodel, but in something else

########################### (iii) Use HDM and Post Lasso and look at results

# Estimating post-lasso
# Females all
postLasso.fem.all.y <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                   high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                   marriagerate + fem_ind_prop,
                                 data = df_y[df_y$H01_01 == 1,])
as.data.frame(postLasso.fem.all.y$selected)
as.numeric(summary(postLasso.fem.all.y))

postLasso.fem.all.o <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                         high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                         marriagerate + fem_ind_prop,
                                       data = df_o[df_o$H01_01 == 1,])
as.data.frame(postLasso.fem.all.o$selected)
as.numeric(summary(postLasso.fem.all.o))

# Females married
postLasso.fem.married.o <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                       high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                       marriagerate + fem_ind_prop,
                                     data = df_o[df_o$H01_01 == 1 & df_o$married == 1,])
as.data.frame(postLasso.fem.married.o$selected)
summary(postLasso.fem.married.o)

postLasso.fem.married.y <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                             high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                             marriagerate + fem_ind_prop,
                                           data = df_y[df_y$H01_01 == 1 & df_y$married == 1,])
as.data.frame(postLasso.fem.married.y$selected)
summary(postLasso.fem.married.y)

# Females single
postLasso.fem.single.y <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                            high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                            marriagerate + fem_ind_prop,
                                          data = df_y[df_y$H01_01 == 1 & df_y$married == 0,])
as.data.frame(postLasso.fem.single.y$selected)
summary(postLasso.fem.single.y)

postLasso.fem.single.o <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                       high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                       marriagerate + fem_ind_prop,
                                     data = df_o[df_o$H01_01 == 1 & df_o$married == 0,])
as.data.frame(postLasso.fem.single.o$selected)
summary(postLasso.fem.single.o)



# Males
postLasso.male.all.y <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                          high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                          marriagerate + fem_ind_prop,
                                        data = df_y[df_y$H01_01 == 2,])
as.data.frame(postLasso.male.all.y$selected)
summary(postLasso.male.all.y)

postLasso.male.all.o <- rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                                       high_n + HSGPER + s1z + migrationrate + women2menratio + 
                                       marriagerate + fem_ind_prop,
                                     data = df_o[df_o$H01_01 == 2,])
as.data.frame(postLasso.male.all.o$selected)
summary(postLasso.male.all.o)

# Aggregating all the parameters from post-Lasso
Sample <- c('fem.all', 'fem.married', 'fem.single', 'males')
Cohort <- rep(c('younger', 'older'), each = 4)
postLasso.coefs <- cbind(Cohort, Sample, 
                          rbind.data.frame(as.numeric(summary(postLasso.fem.all.y)),
                                        as.numeric(summary(postLasso.fem.married.y)),
                                        as.numeric(summary(postLasso.fem.single.y)),
                                        as.numeric(summary(postLasso.male.all.y)),
                                        as.numeric(summary(postLasso.fem.all.o)),
                                        as.numeric(summary(postLasso.fem.married.o)),
                                        as.numeric(summary(postLasso.fem.single.o)),
                                        as.numeric(summary(postLasso.male.all.o))))
colnames(postLasso.coefs)[3:ncol(postLasso.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue')
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




