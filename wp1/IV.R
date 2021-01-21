# IV.R
# Instrumental Variable Specification using RLMS 2018
# Working Paper 1

# install.packages("hdm", repos = "http://R-Forge.R-project.org")
library(hdm)
library(sqldf)
library(plyr); library(dplyr)
library(tidyr)
library(AER)

# Data
# wd
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 

# Some functions 
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

# Transformed variables
rlms18 <- haven::zap_labels(rlms18) 
rlms18$lnwage <- log(rlms18$wage)
rlms18$exper2 <- (rlms18$exper)^2

################################################## Strength of IVs ##############################################

cor.test(rlms18$prestige_family, rlms18$edu_yrs) # 0.28
cor.test(rlms18$edu_family, rlms18$edu_yrs) # 0.35 ok

### Urban/Rural
table(rlms18$STATUS)
rlms18$urban <- ifelse(rlms18$STATUS == 1 | rlms18$STATUS == 2, 1, 0)
table(rlms18$urban)
        
### Estimating 2SLS 
# Running the model for females

# 1st stage
summary(lm(edu_yrs ~ exper + I(exper^2) + urban + edu_family + prestige_family + 
             factor(REGION),
           data = rlms18[rlms18$female == 1,]))

# Generating dummies for the selected regions
rlms18$Permskiy_Krai <- ifelse(rlms18$REGION == 12, 1, 0)
rlms18$Tverskaya_Oblast <- ifelse(rlms18$REGION == 67, 1, 0)
rlms18$Krasnoyarskiy_Kray <- ifelse(rlms18$REGION == 73, 1, 0)
rlms18$Rostovskaya_Oblast <- ifelse(rlms18$REGION == 137, 1, 0)

# Checking
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

# Checking
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


################ STATA df

wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1'
setwd(wd)
library(readstata13)
save.dta13(rlms18, 'rlms18.dta')

### 
# End of file
