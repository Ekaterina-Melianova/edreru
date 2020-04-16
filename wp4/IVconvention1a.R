# IVconvention1a.R

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
library(ivmodel)

##########################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT"
setwd(wd) 

######################################### Data ###########################################################

rst_18 <- read.spss(file="rosstat_18.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
df_18 <- rst_18 %>% select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, H01_04, VZR_RAB, KVZV) %>% mutate (YEAR = 2018)

df_ <- df_18

######################################## Pre-processing: older code ########################################

# Filtering age
table(df_$H01_02)
df <- df_[df_$H01_02 >= 25 & df_$H01_02 < 65,]

# Filtering employed
df <- df[!is.na(df$VZR_RAB),]
table(df$VZR_RAB)

# Education 
table(df$I01_10, df$YEAR)

# 4 categories:
# 0 - lower than secondary
# 1 - secondary 
# 2 - specialized / vocational
# 3 - higher and above

df$edu_4 <- car::recode(df$I01_10, "9=0; 7:8=1; 5:6=2; 1:4=3")
table(df$edu_4, df$YEAR)

# Filtering 3 education levels
df <- df[df$edu_4>0,]

# Education as factor
df$edu_4 <- factor(df$edu_4, levels=c(1,2,3),
                   labels=c("Secondary",
                            "Vocational",
                            "Higher"))

# Wage
df$wage <- df$R_DEN/12
aggregate(wage~YEAR, df, mean)

# Filtering wage > 0 
df <- df %>%
  filter(wage >0)

# Socio-demographics
# Gender
table(df$H01_01)
df$female[df$H01_01==2] <- 1
df$female[df$H01_01==1] <- 0
table(df$female)

# Experience and edu_yrs
df$edu_yrs <- car::recode(df$I01_10, "1=20; 2=17; 3=16; 4=14; 5=12;
                          6=11; 7=11; 8=9")
df$exper <- df$H01_02 - df$edu_yrs - 6
df$exper <- ifelse(df$exper < 0, 0, df$exper)
summary(df$exper)

############ EGE dummy
# In 2018 EGE cohort is aged 22-26, non-EGE cohort  - 27-31
df$ege <- ifelse(df$H01_02 >= 22 & df$H01_02 <= 26, 1,
                 ifelse(df$H01_02 >= 27 & df$H01_02 <= 31, 0, -1))
table(df$ege)

# Filtering 
df_ege <- df[!df$ege == -1, ]

##########################################################################

# Literacy 1897
Grig <- rio::import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.xlsx')
names(Grig)[4] <- 'H00_02'
df$H00_02 <- as.character(df$H00_02)

# Merging with the main df
df <- df %>%
  left_join(Grig[, c('Literacy_97', 'H00_02')], by = 'H00_02')

# Cor
# Whole sample
cor.test(df$edu_yrs, df$Literacy_97) # 0.2
# By age groups
cor.test(df$edu_yrs[df$H01_02 >=25 & df$H01_02 <=35],
         df$Literacy_97[(df$H01_02 >=25) & (df$H01_02 <=35)]) # 0.18
cor.test(df$edu_yrs[df$H01_02 > 35],
         df$Literacy_97[(df$H01_02 > 35)]) # 0.2

# Adding transformed vars
df$lnwage <- log(df$wage)
df$exper2 <- (df$exper)^2

# Cohorts
df_y <- df[df$H01_02 <= 36,]
df_o <- df[df$H01_02 >= 40 & df$H01_02 <= 51,]

# 2SLS with literacy
# Females all
# Younger
ivmodel.fem.all.y <- ivmodel(Y = df_y[df_y$H01_01 == 2, 'lnwage'],
                             D = df_y[df_y$H01_01 == 2, 'edu_yrs'],
                             Z = df_y[df_y$H01_01 == 2, "Literacy_97"],
                             X = df_y[df_y$H01_01 == 2, c('exper', 'exper2')])
ivmodel.fem.all.y

# Older
ivmodel.fem.all.o <- ivmodel(Y = df_o[df_o$H01_01 == 2, 'lnwage'],
                             D = df_o[df_o$H01_01 == 2, 'edu_yrs'],
                             Z = df_o[df_o$H01_01 == 2, 'Literacy_97'],
                             X = df_o[df_o$H01_01 == 2, c('exper', 'exper2')])
ivmodel.fem.all.o

# Males all
# Younger
ivmodel.male.all.y <- ivmodel(Y = df_y[df_y$H01_01 == 1, 'lnwage'],
                             D = df_y[df_y$H01_01 == 1, 'edu_yrs'],
                             Z = df_y[df_y$H01_01 == 1, "Literacy_97"],
                             X = df_y[df_y$H01_01 == 1, c('exper', 'exper2')])
ivmodel.male.all.y

# Older
ivmodel.male.all.o <- ivmodel(Y = df_o[df_o$H01_01 == 1, 'lnwage'],
                             D = df_o[df_o$H01_01 == 1, 'edu_yrs'],
                             Z = df_o[df_o$H01_01 == 1, 'Literacy_97'],
                             X = df_o[df_o$H01_01 == 1, c('exper', 'exper2')])
ivmodel.male.all.o


###########################################################################################################
########################################### 2SLS ##########################################################
###########################################################################################################
library(weights)
library(AER)
########################################### EGE ###########################################################

# is the ege as IV associated with the treatment? strenght of IV
weighted.mean(df_ege$edu_yrs[df_ege$ege==1], df_ege$KVZV[df_ege$ege==1]) # 14.60596
weighted.mean(df_ege$edu_yrs[df_ege$ege==0], df_ege$KVZV[df_ege$ege==0]) # 14.64718

# t-test
wtd.t.test(df_ege$edu_yrs[df_ege$ege==1], df_ege$edu_yrs[df_ege$ege==0],
           df_ege$KVZV[df_ege$ege==1], df_ege$KVZV[df_ege$ege==0], bootse = T)

# The difference is absent --> ege might be not a good IV

### IV regressions with ege
# Females
iv1_fem <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2)|
               exper + I(exper^2) + ege,
               data = df_ege[df_ege$female == 1,], weights = KVZV)
summary(iv1_fem, vcov = sandwich, diagnostics = T)

# Males
iv1_male <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2)|
                  exper + I(exper^2) + ege,
                  data = df_ege[df_ege$female == 0,], weights = KVZV)
summary(iv1_male, vcov = sandwich, diagnostics = T)

# Wu-Hausman test does not allow to reject the null that education is exogenous --> IV does not work here

################################# Employment in female industries #########################################
Sys.setlocale("LC_CTYPE", "russian")

# Loading regional data
wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2'
setwd(wd)
rgvars <- rio::import('rgvars_2.xlsx')
rgvars <- rgvars[,c(1:4, length(rgvars))]
names(rgvars)[1] <- 'H00_02'

# Merging with the main Rosstat 2018 dataframe
df <- df %>%
  left_join(rgvars, by = 'H00_02')

# Is fem_ind_prop as an IV associated with the treatment? strenght of IV
cov.wt(df[, c('edu_yrs', 'fem_ind_prop')], wt = df$KVZV, cor = T)
# cor = -0.147

### IV regressions
# Females
iv2_fem <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2)|
                   exper + I(exper^2) + fem_ind_prop,
                 data = df[df$female == 1,], weights = KVZV)
summary(iv2_fem, vcov = sandwich, diagnostics = T)

# Males
iv2_male <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2)|
                    exper + I(exper^2) + fem_ind_prop,
                  data = df[df$female == 0,], weights = KVZV)
summary(iv2_male, vcov = sandwich, diagnostics = T)

# Testing IV relevance (F-test)

# Females
first_stage <- lm(edu_yrs ~ exper + I(exper^2) + fem_ind_prop,
                  data = df[df$female == 0,], weights = KVZV)
instrFtest <- waldtest(first_stage, lm(edu_yrs ~ exper + I(exper^2),
                                       data = df[df$female == 0,],
                                       weights = KVZV))
instrFtest # fem_ind_prop seems to be a relevant IV for males

# Males
first_stage <- lm(edu_yrs ~ exper + I(exper^2) + fem_ind_prop,
                  data = df[df$female == 1,], weights = KVZV)
instrFtest <- waldtest(first_stage, lm(edu_yrs ~ exper + I(exper^2),
                                       data = df[df$female == 1,], weights = KVZV))
instrFtest # fem_ind_prop seems to be a relevant IV for females

# saving df
saveRDS(df, 'Rosstat18.rds')






