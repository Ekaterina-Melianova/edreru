# IV_Rosstat.R

library(dplyr)
library(tidyr)
library(hdm)
library(lme4)
library(ivmodel)
library(naivereg)
library(rio)
library(npsr)
library(ggplot2)

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
EGE <- rgvars[, c('RoR_names', 's1z', 'OKATO', 'districts')] %>% drop_na()
  
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

df <- na.omit(df)

# Literacy 1897
Grig <- rio::import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.xlsx')
names(Grig)[4] <- 'OKATO'

# Merging with the main df
df <- df %>%
  left_join(Grig[, c('Literacy_97', 'OKATO')], by = 'OKATO')

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

###################################################################################################
###########################  (ii) Use Kang et al ivmodel for running ols, TSLS standard with all IVs;

ivmodel_by_dist <- function(df, if_females = T,
                            marital_status = c('married', 'single', 'all'),
                            females = 'H01_01',
                            married = 'married',
                            districts = 'districts',
                            Y = 'lnwage',
                            D = 'edu_yrs',
                            Z = c('high_n', 'HSGPER', 's1z', 'migrationrate',
                                  'women2menratio', 'marriagerate', 'fem_ind_prop'),
                            X = c('exper', 'exper2')){
# Setting a vector with unique federal districts
dist_vec <- unique(df[, districts])
ivmodel_list_res <- list()

# ivmodel for each combination of of gender and marital status
if (if_females == T & marital_status == 'all'){
  for (i in 1:length(dist_vec)){
    ivmodel_list_res[[i]] <- ivmodel(Y = df[df[, females] == 1 & df[, districts] == dist_vec[i], Y],
                                    D = df[df[, females] == 1 & df[, districts] == dist_vec[i], D],
                                    Z = df[df[, females] == 1 & df[, districts] == dist_vec[i], Z],
                                    X = df[df[, females] == 1 & df[, districts] == dist_vec[i], X])
    
  } 
}
else if (if_females == T & marital_status == 'married'){
  for (i in 1:length(dist_vec)){
    ivmodel_list_res[[i]] <- ivmodel(Y = df[df[, females] == 1 & df[, married] == 1 & df[, districts] == dist_vec[i], Y],
                                    D = df[df[, females] == 1 & df[, married] == 1 & df[, districts] == dist_vec[i], D],
                                    Z = df[df[, females] == 1 & df[, married] == 1 & df[, districts] == dist_vec[i], Z],
                                    X = df[df[, females] == 1 & df[, married] == 1 & df[, districts] == dist_vec[i], X])
    
  }
}
else if (if_females == T & marital_status == 'single'){
  for (i in 1:length(dist_vec)){
    ivmodel_list_res[[i]] <- ivmodel(Y = df[df[, females] == 1 & df[, married] == 0 & df[, districts] == dist_vec[i], Y],
                                    D = df[df[, females] == 1 & df[, married] == 0 & df[, districts] == dist_vec[i], D],
                                    Z = df[df[, females] == 1 & df[, married] == 0 & df[, districts] == dist_vec[i], Z],
                                    X = df[df[, females] == 1 & df[, married] == 0 & df[, districts] == dist_vec[i], X])
    
  }
}
else if (if_females == F & marital_status == 'all'){
  for (i in 1:length(dist_vec)){
    ivmodel_list_res[[i]] <- ivmodel(Y = df[df[, females] == 2 & df[, districts] == dist_vec[i], Y],
                                    D = df[df[, females] == 2 & df[, districts] == dist_vec[i], D],
                                    Z = df[df[, females] == 2 & df[, districts] == dist_vec[i], Z],
                                    X = df[df[, females] == 2 & df[, districts] == dist_vec[i], X])
    
  }
}
else if (if_females == F & marital_status == 'married'){
  for (i in 1:length(dist_vec)){
    ivmodel_list_res[[i]] <- ivmodel(Y = df[df[, females] == 2 & df[, married] == 1 & df[, districts] == dist_vec[i], Y],
                                    D = df[df[, females] == 2 & df[, married] == 1 & df[, districts] == dist_vec[i], D],
                                    Z = df[df[, females] == 2 & df[, married] == 1 & df[, districts] == dist_vec[i], Z],
                                    X = df[df[, females] == 2 & df[, married] == 1 & df[, districts] == dist_vec[i], X])
    
  }
}
else if (if_females == F & marital_status == 'single'){
  for (i in 1:length(dist_vec)){
    ivmodel_list_res[[i]] <- ivmodel(Y = df[df[, females] == 2 & df[, married] == 0 & df[, districts] == dist_vec[i], Y],
                                    D = df[df[, females] == 2 & df[, married] == 0 & df[, districts] == dist_vec[i], D],
                                    Z = df[df[, females] == 2 & df[, married] == 0 & df[, districts] == dist_vec[i], Z],
                                    X = df[df[, females] == 2 & df[, married] == 0 & df[, districts] == dist_vec[i], X])
    
    }
}
return(ivmodel_list_res)
}
# Females all
# Younger/older
ivmodel.fem.all.y <- ivmodel_by_dist(df = df_y, if_females = T, marital_status = 'all')
ivmodel.fem.all.o <- ivmodel_by_dist(df = df_o, if_females = T, marital_status = 'all')

# Females married
# Younger/older
ivmodel.fem.married.y <- ivmodel_by_dist(df = df_y, if_females = T, marital_status = 'married')
ivmodel.fem.married.o <- ivmodel_by_dist(df = df_o, if_females = T, marital_status = 'married')

# Females single
# Younger/older
ivmodel.fem.single.y <- ivmodel_by_dist(df = df_y, if_females = T, marital_status = 'single')
ivmodel.fem.single.o <- ivmodel_by_dist(df = df_o, if_females = T, marital_status = 'single')

# Males all
# Younger/older
ivmodel.male.all.y <- ivmodel_by_dist(df = df_y, if_females = F, marital_status = 'all')
ivmodel.male.all.o <- ivmodel_by_dist(df = df_o, if_females = F, marital_status = 'all')

################################################# Extracting Coefs
Estimator <-  rep(c( 'OLS', 'TSLS'), 8)
District <- rep(unique(df$districts), each = 2)
list_with_est <- list()

model_estimated_name <- c('ivmodel.fem.all.y', 'ivmodel.fem.all.o', 'ivmodel.fem.married.y', 
                          'ivmodel.fem.married.o', 'ivmodel.fem.single.y', 'ivmodel.fem.single.o',
                          'ivmodel.male.all.y', 'ivmodel.male.all.o')

for (i in 1:length(model_estimated_name)){
  list_with_est[[i]] <- eval(parse(text = paste0('cbind.data.frame(District, Estimator, round(do.call(rbind.data.frame, lapply(1:length(',
                                                 model_estimated_name[i], ')',
                                                 ', function(i) cbind.data.frame(',
                                                 paste0(model_estimated_name[i], '[[i]]$kClass))),3))', sep = ''))))
  rownames(list_with_est[[i]]) <- NULL
  colnames(list_with_est[[i]]) <- c('District','Estimator','Est', 'SE', 'test_stat', 'pvalue', 'ci2.5', 'ci97.5')
  
}

#########################################################################################################################
########################################## lm
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
#########################################################################################################################

########################### (iii) Use HDM and Post Lasso and look at results
# Formulas
fm_postLasso <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + high_n + HSGPER + s1z +
                migrationrate + women2menratio + marriagerate + fem_ind_prop)
fm_OLS <- formula(log(wage) ~ edu_yrs + exper + I(exper^2))
# Subsets
subset <-  c('df_y[df_y$H01_01 == 1,]', # females younger
             'df_o[df_o$H01_01 == 1,]', # females older
             'df_y[df_y$H01_01 == 1 & df_y$married == 1,]', # females younger married
             'df_o[df_o$H01_01 == 1 & df_o$married == 1,]', # females older married
             'df_y[df_y$H01_01 == 1 & df_y$married == 0,]', # females younger signle
             'df_o[df_o$H01_01 == 1 & df_o$married == 0,]', # females older single
             'df_y[df_y$H01_01 == 2,]', # males younger
             'df_o[df_o$H01_01 == 2,]') # males older

# Function for a series of postLasso and OLS
postLasso <- function(fm_postLasso, fm_OLS, subset){
  postLasso <- list()
  OLS <- list()
  # running rlassoIVselectZ for each subset (group of interest)
  for (i in 1:length(subset)){
    postLasso[[i]] <- rlassoIVselectZ(fm_postLasso, data = eval(parse(text = paste(subset[i]))))
    OLS[[i]] <- lm(fm_OLS, data = eval(parse(text = paste(subset[i]))))
  }
  # resulting list with both postLasso and OLS
  res <- list(postLasso, OLS)
  names(res) <- c('postLasso', 'OLS')
  return(res)
}

# Running the function
postLasso_OLS <- postLasso(fm_postLasso = fm_postLasso, fm_OLS = fm_OLS, subset = subset)
names(postLasso_OLS)

# Aggregating all the parameters
Sample <- rep(c('females all', 'females married', 'females single', 'males'),  each = 2, 2)
Cohort <- rep(c('Young', 'Older', 'Young', 'Older'), 8)
Method <- rep(c('pLasso', 'OLS'), each = 8)
postLasso.coefs <- eval(parse(text = paste0('cbind(Method, Cohort, Sample, mapply(c,',
                        paste0('c(as.numeric(summary(postLasso_OLS$postLasso[[', 1:8, ']])),',
                        'confint(postLasso_OLS$postLasso[[', 1:8, ']]))', collapse = ', '), ',',
                        paste0('c(summary(postLasso_OLS$OLS[[', 1:8, ']])$coefficients["edu_yrs",],',
                        'confint(postLasso_OLS$OLS[[', 1:8, ']])["edu_yrs",])', collapse = ', '), '))')))
# Naming
colnames(postLasso.coefs)[4:ncol(postLasso.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'lower', 'upper')
# Rounding
postLasso.coefs[, 4:ncol(postLasso.coefs)] <- sapply(4:ncol(postLasso.coefs),
                                                     function(i) round(as.numeric(postLasso.coefs[,i]), 3))
# A matrix to data.frame
postLasso.coefs <- as.data.frame(postLasso.coefs)

# Estimates as numeric
postLasso.coefs$Est <- as.numeric(as.character(postLasso.coefs$Est))
postLasso.coefs$lower <- as.numeric(as.character(postLasso.coefs$lower))
postLasso.coefs$upper <- as.numeric(as.character(postLasso.coefs$upper))

# A unique identifier Cohort_Sample
postLasso.coefs$cat <- paste0(postLasso.coefs$Method, ' ', postLasso.coefs$Cohort, '_', postLasso.coefs$Sample)
postLasso.coefs$Cohort_Sample <- paste0(postLasso.coefs$Cohort, ' ', postLasso.coefs$Sample)

# Arranging
postLasso.coefs <- postLasso.coefs %>%
  group_by(Method) %>%
  arrange(Est)

# Splitting the df by method
pLasso_coefs <- postLasso.coefs[postLasso.coefs$Method == 'pLasso', c('Method', 'Cohort_Sample', 'Est', 'lower', 'upper')]
OLS_coefs <- postLasso.coefs[postLasso.coefs$Method == 'OLS', c('Method', 'Cohort_Sample', 'Est', 'lower', 'upper')]

# Creating a sorted variable for Cohort_Sample
pLasso_coefs$cat_sorted <- factor(pLasso_coefs$Cohort_Sample, levels = pLasso_coefs$Cohort_Sample)
OLS_coefs$cat_sorted <- factor(OLS_coefs$Cohort_Sample, levels = pLasso_coefs$cat_sorted)


#################################################################################################
#################################################################################################
#################################################################################################

####################################### Literacy 1897

# Cor
# Whole sample
cor.test(df$edu_yrs, df$Literacy_97) # 0.23
# By age groups
cor.test(df_y$edu_yrs, df_y$Literacy_97) # 0.23
cor.test(df_o$edu_yrs, df_o$Literacy_97) # 0.25

# 2SLS with literacy
ivmodel_series <- function(df, if_females = T,
                            marital_status = c('married', 'single', 'all'),
                            females = 'H01_01',
                            married = 'married',
                            Y = 'lnwage',
                            D = 'edu_yrs',
                            Z = 'Literacy_97',
                            X = c('exper', 'exper2')){
# ivmodel for each combination of of gender and marital status
if (if_females == T & marital_status == 'all'){
   ivmodel_list_res <- ivmodel(Y = df[df[, females] == 1, Y],
                                    D = df[df[, females] == 1, D],
                                    Z = df[df[, females] == 1, Z],
                                    X = df[df[, females] == 1, X])
 }
else if (if_females == T & marital_status == 'married'){
   ivmodel_list_res <- ivmodel(Y = df[df[, females] == 1 & df[, married] == 1, Y],
                                    D = df[df[, females] == 1 & df[, married] == 1, D],
                                    Z = df[df[, females] == 1 & df[, married] == 1, Z],
                                    X = df[df[, females] == 1 & df[, married] == 1, X])
 }
else if (if_females == T & marital_status == 'single'){
   ivmodel_list_res <- ivmodel(Y = df[df[, females] == 1 & df[, married] == 0, Y],
                                    D = df[df[, females] == 1 & df[, married] == 0, D],
                                    Z = df[df[, females] == 1 & df[, married] == 0, Z],
                                    X = df[df[, females] == 1 & df[, married] == 0, X])
 }
else if (if_females == F & marital_status == 'all'){
   ivmodel_list_res <- ivmodel(Y = df[df[, females] == 2, Y],
                                    D = df[df[, females] == 2, D],
                                    Z = df[df[, females] == 2, Z],
                                    X = df[df[, females] == 2, X])
 }
else if (if_females == F & marital_status == 'married'){
   ivmodel_list_res <- ivmodel(Y = df[df[, females] == 2 & df[, married] == 1, Y],
                                    D = df[df[, females] == 2 & df[, married] == 1, D],
                                    Z = df[df[, females] == 2 & df[, married] == 1, Z],
                                    X = df[df[, females] == 2 & df[, married] == 1, X])
 }
else if (if_females == F & marital_status == 'single'){
   ivmodel_list_res <- ivmodel(Y = df[df[, females] == 2 & df[, married] == 0, Y],
                                    D = df[df[, females] == 2 & df[, married] == 0, D],
                                    Z = df[df[, females] == 2 & df[, married] == 0, Z],
                                    X = df[df[, females] == 2 & df[, married] == 0, X])
  } 
return(ivmodel_list_res)
}

# Females all
# Younger/older
ivmodel.fem.all.y <- ivmodel_series(df = df_y, if_females = T, marital_status = 'all')
ivmodel.fem.all.o <- ivmodel_series(df = df_o, if_females = T, marital_status = 'all')

# Females married
# Younger/older
ivmodel.fem.married.y <- ivmodel_series(df = df_y, if_females = T, marital_status = 'married')
ivmodel.fem.married.o <- ivmodel_series(df = df_o, if_females = T, marital_status = 'married')

# Females single
# Younger/older
ivmodel.fem.single.y <- ivmodel_series(df = df_y, if_females = T, marital_status = 'single')
ivmodel.fem.single.o <- ivmodel_series(df = df_o, if_females = T, marital_status = 'single')

# Males all
# Younger/older
ivmodel.male.all.y <- ivmodel_series(df = df_y, if_females = F, marital_status = 'all')
ivmodel.male.all.o <- ivmodel_series(df = df_o, if_females = F, marital_status = 'all')

################################################# Extracting Coefs
# A vector with models' names
model_estimated_name <- c('ivmodel.fem.all.y', 'ivmodel.fem.all.o', 'ivmodel.fem.married.y', 
                          'ivmodel.fem.married.o', 'ivmodel.fem.single.y', 'ivmodel.fem.single.o',
                          'ivmodel.male.all.y', 'ivmodel.male.all.o')

# Extracting estimations from each model and creating a dataframe
ivmodel_est <- c()
for (i in 1:length(model_estimated_name)){
ivmodel_est <- eval(parse(text = paste0('rbind(ivmodel_est, do.call(cbind.data.frame,',
                                 'lapply(1:length(', model_estimated_name[i], '$kClass),',
                                 'function(x) cbind(', model_estimated_name[i], '$kClass[[x]]))))')))
}

# Adding Sample, Cohort, Estimator
rownames(ivmodel_est) <- NULL
Method <-  rep(c( 'OLS', 'TSLS'), 8)
Sample <- rep(c('females all', 'females married', 'females single', 'males'),  each = 4)
Cohort <- rep(c('Young', 'Young', 'Older', 'Older'), 4) 
ivmodel_est <- cbind.data.frame(Method, Sample, Cohort, ivmodel_est)

# Naming
colnames(ivmodel_est) <- c('Method', 'Sample', 'Cohort', 'Est', 'SE', 'test_stat', 'pvalue', 'lower', 'upper')

# Rounding
ivmodel_est[, 4:ncol(ivmodel_est)] <- sapply(4:ncol(ivmodel_est),
                                                     function(i) round(as.numeric(ivmodel_est[,i]), 3))
# A unique identifier Cohort_Sample
ivmodel_est$cat <- paste0(ivmodel_est$Method, ' ', ivmodel_est$Cohort, '_', ivmodel_est$Sample)
ivmodel_est$Cohort_Sample <- paste0(ivmodel_est$Cohort, ' ', ivmodel_est$Sample)

# Arranging
ivmodel_est <- ivmodel_est %>%
  group_by(Method) %>%
  arrange(Est)

# Splitting the df by method
TSLS_coefs <- ivmodel_est[ivmodel_est$Method == 'TSLS', c('Method', 'Cohort_Sample', 'Est', 'lower', 'upper')]

# Creating a sorted variable for Cohort_Sample
TSLS_coefs$cat_sorted <- factor(TSLS_coefs$Cohort_Sample, levels = pLasso_coefs$Cohort_Sample)

# Merging pLasso and TSLS (on literacy)
pLasso_TSLS_coefs <- rbind.data.frame(pLasso_coefs, TSLS_coefs, OLS_coefs)

# Plotting coefficients

# postLasso
ggplot(pLasso_TSLS_coefs, aes(y = cat_sorted, colour = Method)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, size = 1) + 
  scale_color_manual(values = c('darkgreen', 'blue', 'red'), 
                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate', 
                                '2SLS: Literacy_97')) +
  geom_point(aes(x = Est), size = 2) +
  geom_text(aes(x = Est, label = Est, vjust = -1), color="black" ) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot')

# OLS
#g2 <- ggplot(OLS_coefs, aes(y = cat_sorted)) + 
#  geom_errorbar(aes(xmin = lower, xmax = upper),
#                width = 0, size = 1, colour = 'maroon') + 
#  geom_point(aes(x = Est), size = 3, color = 'maroon') +
#  geom_text(aes(x = Est, label = Est, vjust = -1), color="black" ) +
#  theme(panel.background = element_blank(),
#        axis.title = element_blank(),
#        axis.text = element_text(size = 14, face = 'bold'), 
#        axis.line = element_line(color = 'black')) +
#  scale_x_continuous(limits = c(0.08, 0.15), breaks = c(0.08, 0.1, 0.12, 0.14)) +
#  ggtitle('OLS')


# 2SLS
#g3 <- ggplot(TSLS_coefs, aes(y = cat_sorted)) + 
#  geom_errorbar(aes(xmin = lower, xmax = upper),
#                width = 0, size = 1, colour = 'maroon') + 
#  geom_point(aes(x = Est), size = 3, color = 'maroon') +
#  geom_text(aes(x = Est, label = Est, vjust = -1), color="black" ) +
#  theme(panel.background = element_blank(),
#        axis.title = element_blank(),
#        axis.text = element_text(size = 14, face = 'bold'), 
#        axis.line = element_line(color = 'black'),
#        plot.title.position = 'plot') +
#  ggtitle('TSLS: Literacy 1897')

# gridExtra::grid.arrange(g1, g2, nrow = 1, ncol = 2)
