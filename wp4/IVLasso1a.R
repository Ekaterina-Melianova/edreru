# IVLasso1a.R
# Testing post-Lasso on Rosstat and RLMS datasets

# install.packages("hdm", repos = "http://R-Forge.R-project.org")
library(hdm)
library(sqldf)
library(plyr); library(dplyr)
library(tidyr)

############################################################################################################
############################################## Rosstat #####################################################
############################################################################################################

# Loading data
wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4'
setwd(wd)
df <- readRDS('Rosstat18.rds')

# Females
postLasso1.fem = rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|
                               exper + I(exper^2) + fem_ind_prop*ege,
                             data = df[df$female == 1,])
summary(postLasso1.fem)
confint(postLasso1.fem)
postLasso1.fem$selected

# Males
postLasso1.male = rlassoIVselectZ(log(wage) ~ edu_yrs + exper + I(exper^2)|
                               exper + I(exper^2) + fem_ind_prop*ege,
                             data = df[df$female == 0,])
summary(postLasso1.male)
confint(postLasso1.male)
postLasso1.male$selected

############################################################################################################
################################################# RLMS #####################################################
############################################################################################################

# wd
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 
# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")
# Connecting with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")

############################################################################################################

### SES variables
# J216AC08 - father's occupation at a person's age of 15
# J216BC08 - mother's occupation at a person's age of 15
# J216AC08 - father's education at a person's age of 15
# J216AC08 - mother's education at a person's age of 15

ses_ <- selectFromSQL(c("IDIND", "YEAR", "J216AC08", "J216BC08", "J217A", "J217B", "AGE"))
dbDisconnect(db)
ses_ <- SysMisFix(ses_) # determining system missings

# Droping the waves when the questions were not asked (system missings)
ses <- ses_ %>% drop_na()

### Looking at the remained user-defined missings
tail(table(ses$J216AC08), 7) 
tail(table(ses$J216BC08), 7)
tail(table(ses$J217A), 5)
tail(table(ses$J217B), 5)

# Types of missings:
# 92 Подготовительные курсы по нескольким специальностям - this value was not observed
# 93 РЕСПОНДЕНТУ МЕНЬШЕ 15 ЛЕТ, вопрос не задавался
# 94 НЕ ЗНАЕТ, В ТО ВРЕМЯ НЕ ЖИЛ ВМЕСТЕ С ОТЦОМ/МАТЕРЬЮ
# 95 ОТЕЦ/МАТЬ НЕ РАБОТАЛ/А В ТО ВРЕМЯ
# 96 ОТЕЦ/МАТЬ УЖЕ УМЕР/ЛА
# 97 ЗАТРУДНЯЮСЬ ОТВЕТИТЬ
# 98 ОТКАЗ ОТ ОТВЕТА
# 99 НЕТ ОТВЕТА

missings <- c(99999993, 99999994, 99999995, 99999996, 99999997, 99999998, 99999999)

############################################## OCCUPATIONS #################################################

# Making occupations a numeric variable using The Standard Occupational Prestige Scale (SIOPS)
library(ISCO08ConveRsions)

# Making everything 4-digit 
ses$occ_father <- ifelse(ses$J216AC08 %in% missings, ses$J216AC08,
                         ifelse(nchar(ses$J216AC08) == 1, paste0(ses$J216AC08, '000'),
                                ifelse(nchar(ses$J216AC08) == 2, paste0(ses$J216AC08, '00'),
                                       ifelse(nchar(ses$J216AC08) == 3, paste0(ses$J216AC08, '0'), ses$J216AC08))))

ses$occ_mother <- ifelse(ses$J216BC08 %in% missings, ses$J216BC08,
                         ifelse(nchar(ses$J216BC08) == 1, paste0(ses$J216BC08, '000'),
                                ifelse(nchar(ses$J216BC08) == 2, paste0(ses$J216BC08, '00'),
                                       ifelse(nchar(ses$J216BC08) == 3, paste0(ses$J216BC08, '0'), ses$J216BC08))))

# Applying the isco08tosiops08 function to each element in the data

# Father
prestige_father <- c()
for (i in 1:nrow(ses)){
  if (ses[i, 'occ_father'] %in% missings){
    prestige_father <- append(prestige_father, 0)
  }
  else{
    prestige_father <- append(prestige_father, isco08tosiops08(ses[i, 'occ_father']))
  }
}

# Mother
prestige_mother <- c()
for (i in 1:nrow(ses)){
  if (ses[i, 'occ_mother'] %in% missings){
    prestige_mother <- append(prestige_mother, 0)
  }
  else{
    prestige_mother <- append(prestige_mother, isco08tosiops08(ses[i, 'occ_mother']))
  }
}

# Adding parental prestige to the main df
ses <- cbind.data.frame(ses, prestige_father, prestige_mother)

# Taking max for the family prestige
ses$prestige_family <- apply(cbind.data.frame(ses$prestige_father, ses$prestige_mother), 1, max)

############################################## EDUCATION ##################################################

# Mother
ses$edu_yrs_mother<-car::recode(ses$J217A,"1=0; 2=4; 3=8; 4=12; 5=11; 6=12;
                               7=11; 8=12; 9=16; 10=18; 11=21; 12=6; 99999993=NA; else=0")

summary(ses$edu_yrs_mother)

# Father
ses$edu_yrs_father<-car::recode(ses$J217B,"1=0; 2=4; 3=8; 4=12; 5=11; 6=12;
                               7=11; 8=12; 9=16; 10=18; 11=21; 12=6; 99999993=NA; else=0")

table(ses$edu_yrs_father, ses$edu_yrs_mother)

# dropping those who were not asked (14-year-olds and younger)
ses <- ses %>% drop_na()

# Family educ: max
ses$edu_family <- apply(cbind.data.frame(ses$edu_yrs_father, ses$edu_yrs_mother), 1, max)

######################################### Birth Year Dummies #############################################
# Dummy set for a birth year
#ses$birth_ <- ifelse(ses$YEAR == 2006, 2006 - ses$AGE, 2011 - ses$AGE) 
#ses$birth_ <- as.factor(ses$birth_)
#ses <- cbind(ses, model.matrix( ~ birth_ - 1, data = ses))
#ses_fin <- ses[, c(2, 6, 11, 16, 19:ncol(ses))]
ses_fin <- ses[, c(2, 6, 16, 19)]
names(ses_fin)[2] <- 'YEAR_ses'

# Renaming AGE not to confuse it further in 2018
#names(ses_fin)[3] <- 'AGE_2006_2011'

### Main RLMS data
rlms <- readRDS('C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/df_mincer.rds')

# Filtering 2018 
rlms18 <- filter(rlms, YEAR == 2018)




##########################################################################
# Region
Region_rlms <- selectFromSQL(c("IDIND", "YEAR", "REGION", "AGE", 'STATUS')) %>% filter(YEAR == 2018)

# Merging
rlms18 <- rlms18 %>%
  left_join(Region_rlms[,c("IDIND", "YEAR", "REGION", "AGE", 'STATUS')], by = c("IDIND", "YEAR"))

# Literacy 1897
#Grig <- rio::import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.xlsx')
#Grig <- na.omit(Grig)
#names(Grig)[5] <- 'REGION'

# Merging with rlms18
#rlms18_Grig <- rlms18 %>%
#  left_join(Grig[, c('Literacy_97', 'REGION')], by = 'REGION')

# Cor
# Whole sample
#cor.test(rlms18_Grig$edu_yrs, rlms18_Grig$Literacy_97, na.action = no.omit) # 0.07740857 
# By age groups
#cor.test(rlms18_Grig$edu_yrs[rlms18_Grig$AGE >=25 & rlms18_Grig$AGE <=35],
#         rlms18_Grig$Literacy_97[(rlms18_Grig$AGE >=25) & (rlms18_Grig$AGE <=35)],
#         na.action = no.omit) # 0.06750083
#cor.test(rlms18_Grig$edu_yrs[rlms18_Grig$AGE > 35],
#         rlms18_Grig$Literacy_97[(rlms18_Grig$AGE > 35)],
#         na.action = no.omit) # 0.0844305

##########################################################################




# Merging ses with the 2018 RLMS data
rlms18_ <- rlms18 %>%
  left_join(ses_fin, by = 'IDIND')

### Filtering IDIND (if a person was serveyed 2 times on ses questions in 2006 and 2011,
# selecting 2006 as the earliest)

# Sorting
rlms18_ <- rlms18_ %>%
  arrange(IDIND, YEAR_ses)

# Removing duplicates (i.e., year 2011 among those who were surveyed 2 times)
dup <- duplicated(rlms18_$IDIND)
rlms18_ <- rlms18_[dup == F, ]

# Removing those who were surveyed neither in 2006 nor in 2011 (i.e. rows with NA)
rlms18 <- rlms18_ %>% drop_na()

### Further cleaning
# Droping birth dummies that lost variation due to the preceding filtering
summary(rlms18) # those are 1946 - 1952, 1994 - 1997
#rlms18 <- rlms18[, !names(rlms18) %in% paste0("birth_19", c(46:52, 94:97))]

# Generating age
#rlms18$age <- 2018 - rlms18$birth_year
#summary(rlms18$age)
#
## Birth decades
#
#paste(paste0("rlms18$birth_19", 53:59), '==1', collapse = '|') # manually pasting this expression to ifelse()
#rlms18$birth_50 <- ifelse(rlms18$birth_1950 ==1|rlms18$birth_1951 ==1|
#                            rlms18$birth_1952 ==1|rlms18$birth_1953 ==1|rlms18$birth_1954 ==1|rlms18$birth_1955 ==1|
#                            rlms18$birth_1956 ==1|rlms18$birth_1957 ==1|rlms18$birth_1958 ==1|rlms18$birth_1959 ==1, 1, 0)
#
#paste(paste0("rlms18$birth_19", 60:69), '==1', collapse = '|')
#rlms18$birth_60 <- ifelse(rlms18$birth_1960 ==1|rlms18$birth_1961 ==1|
#                            rlms18$birth_1962 ==1|rlms18$birth_1963 ==1|rlms18$birth_1964 ==1|rlms18$birth_1965 ==1|
#                            rlms18$birth_1966 ==1|rlms18$birth_1967 ==1|rlms18$birth_1968 ==1|rlms18$birth_1969 ==1, 1, 0)
#
#paste(paste0("rlms18$birth_19", 70:79), '==1', collapse = '|')
#rlms18$birth_70 <- ifelse(rlms18$birth_1970 ==1|rlms18$birth_1971 ==1|
#                            rlms18$birth_1972 ==1|rlms18$birth_1973 ==1|rlms18$birth_1974 ==1|rlms18$birth_1975 ==1|
#                            rlms18$birth_1976 ==1|rlms18$birth_1977 ==1|rlms18$birth_1978 ==1|rlms18$birth_1979 ==1, 1, 0)
#
#paste(paste0("rlms18$birth_19", 80:89), '==1', collapse = '|')
#rlms18$birth_80 <- ifelse(rlms18$birth_1980 ==1|rlms18$birth_1981 ==1|
#                            rlms18$birth_1982 ==1|rlms18$birth_1983 ==1|rlms18$birth_1984 ==1|rlms18$birth_1985 ==1|
#                            rlms18$birth_1986 ==1|rlms18$birth_1987 ==1|rlms18$birth_1988 ==1|rlms18$birth_1989 ==1, 1, 0)
#
#paste(paste0("rlms18$birth_19", 90:93), '==1', collapse = '|')
#rlms18$birth_90 <- ifelse(rlms18$birth_1990 ==1|rlms18$birth_1991 ==1|rlms18$birth_1992 ==1|rlms18$birth_1993 ==1, 1, 0)
#
#
#table(rlms18$birth_50)
#table(rlms18$birth_60)
#table(rlms18$birth_70)
#table(rlms18$birth_80)
#table(rlms18$birth_90)

# Transformed variables
rlms18 <- haven::zap_labels(rlms18) 
rlms18$lnwage <- log(rlms18$wage)
rlms18$exper2 <- (rlms18$exper)^2

################################################## Region #######################################################

# Connecting with SQLite
#db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")
#region_ <- selectFromSQL(c("IDIND", "YEAR", "REGION"))
#dbDisconnect(db)
#region_ <- SysMisFix(region_)
#region <- region_ %>% filter(YEAR == 2018) %>% select(c("IDIND", "REGION"))
#region$REGION <- as.factor(region$REGION)
#
## Merging with the main data
#rlms18 <- rlms18 %>%
#  left_join(region, by = 'IDIND')

################################################## Strenght of IVs ##############################################

cor.test(rlms18$prestige_family, rlms18$edu_yrs) # 0.28
cor.test(rlms18$edu_family, rlms18$edu_yrs) # 0.35 ok

################################################## Lasso ########################################################

# Creating a combination of instruments for interactions
#ses <- c("prestige_family","edu_family", 'factor(REGION)')
## birth <- paste0("birth_19", 53:92) # 93 - ref category
#birth_dec <- paste0("birth_", seq(50, 80, 10)) # 90 - ref category
#ses_birth <- expand.grid(ses, birth_dec, stringsAsFactors = FALSE)
#
## Interactions 
#zinteract <- paste(do.call(paste, c(ses_birth, sep="*")), collapse = " + ")
#
## Formula
#forminteract <-  formula(paste("log(wage) ~ ", "edu_yrs + exper + I(exper^2)",
#                             "|", "exper + I(exper^2) + prestige_family*edu_family + factor(REGION)*prestige_family + factor(REGION)*edu_family +",
#                             zinteract, sep = ""))
#
#### Estimating post-lasso
## Females
#postLasso.fem <- rlassoIVselectZ(forminteract, data = rlms18[rlms18$female == 1,])
#selected.fem <- as.data.frame(postLasso.fem$selected)
#summary(postLasso.fem)
#confint(postLasso.fem)
#
## Males
#postLasso.male <- rlassoIVselectZ(forminteract, data = rlms18[rlms18$female == 0,])
#selected.mal <- as.data.frame(postLasso.male$selected)
#summary(postLasso.male)
#confint(postLasso.male)
#

### Urban/Rural
table(rlms18$STATUS)
rlms18$urban <- ifelse(rlms18$STATUS == 1 | rlms18$STATUS == 2, 1, 0)
table(rlms18$urban)
        
################################################################# 
### Estimating 2SLS 

library(AER)

# Running the model for females

# 1st stage
summary(lm(edu_yrs ~ exper + I(exper^2) + urban + edu_family + prestige_family + 
             factor(REGION),
           data = rlms18[rlms18$female == 1,]))

# generating dummies for the selected regions
rlms18$Permskiy_Krai <- ifelse(rlms18$REGION == 12, 1, 0)
#rlms18$region48 <- ifelse(rlms18$REGION == 48, 1, 0)
rlms18$Tverskaya_Oblast <- ifelse(rlms18$REGION == 67, 1, 0)
rlms18$Krasnoyarskiy_Kray <- ifelse(rlms18$REGION == 73, 1, 0)
#rlms18$region129 <- ifelse(rlms18$REGION == 129, 1, 0)
rlms18$Rostovskaya_Oblast <- ifelse(rlms18$REGION == 137, 1, 0)

# Cheking
summary(lm(edu_yrs ~ exper + I(exper^2) + urban + edu_family + prestige_family +
             Permskiy_Krai + Tverskaya_Oblast + Krasnoyarskiy_Kray + Rostovskaya_Oblast,
           data = rlms18[rlms18$female == 1,]))

# IV
twoSLS.fem <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2) + urban|
                    exper + I(exper^2) + urban +
                    edu_family + 
                    prestige_family +
                    Permskiy_Krai + Tverskaya_Oblast + Krasnoyarskiy_Kray + Rostovskaya_Oblast,
                  data = rlms18[rlms18$female == 1,])
summary(twoSLS.fem, vcov = sandwich, diagnostics = T) 



# Running the model for males

# 1st stage
summary(lm(edu_yrs ~ exper + I(exper^2) + urban + edu_family + prestige_family + 
             factor(REGION),
           data = rlms18[rlms18$female == 0,]))

#rlms18$region12 <- ifelse(rlms18$REGION == 12, 1, 0)
rlms18$Tambovskaya_Oblast <- ifelse(rlms18$REGION == 33, 1, 0)
rlms18$Kabardino_Balkarskaya_Resp <- ifelse(rlms18$REGION == 77, 1, 0)

# Cheking
summary(lm(edu_yrs ~ exper + I(exper^2) + urban + edu_family + prestige_family +
             Permskiy_Krai + Tambovskaya_Oblast + Kabardino_Balkarskaya_Resp,
           data = rlms18[rlms18$female == 0,]))

# IV
twoSLS.mal <- ivreg(log(wage) ~ edu_yrs + exper + I(exper^2) + urban|
                      exper + I(exper^2) + urban +
                      edu_family +
                      prestige_family +
                      Permskiy_Krai + Tambovskaya_Oblast + Kabardino_Balkarskaya_Resp,
                    data = rlms18[rlms18$female == 0,])
summary(twoSLS.mal, vcov = sandwich, diagnostics = T) 





################33 STATA df
wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4'
setwd(wd)
library(readstata13)
save.dta13(rlms18, 'rlms18.dta')
