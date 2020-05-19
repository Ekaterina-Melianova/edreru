# rosstat7a.R

# Map asked by Harry 

library(foreign)
library(plyr); library(dplyr)
library(gmodels)
library(lmtest)
library(sqldf)
library("XLConnectJars",lib.loc="C:/Users/wb164718/Documents/R/win-library/3.5")
library(questionr)
library(labelled)
library(tidyr)
library(magrittr)
library(ggplot2)
library(data.table)
library(pbapply)
library(gridExtra)
library(stargazer)
library(xtable)

##########################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT"
setwd(wd) 

######################################### Data ###########################################################

rst_18 <- read.spss(file="rosstat_18.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_17 <- read.spss(file="rosstat_17.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_16 <- read.spss(file="rosstat_16.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_15 <- read.spss(file="rosstat_15.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_14 <- read.spss(file="rosstat_14.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)

df_18 <- rst_18 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2018)
df_17 <- rst_17 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2017)
df_16 <- rst_16 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2016)
df_15 <- rst_15 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2015)
df_14 <- rst_14 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2014)

df_ <- rbind(df_14, df_15, df_16, df_17, df_18)

############################################################################################################

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

# Experience (naive)
df$edu_yrs <- car::recode(df$I01_10, "1=20; 2=17; 3=16; 4=14; 5=12;
                            6=11; 7=11; 8=9")
df$exper <- df$H01_02 - df$edu_yrs - 6
df$exper <- ifelse(df$exper < 0, 0, df$exper)
summary(df$exper)

df_18 <- df[df$YEAR == 2018,]
saveRDS(df_18, 'Rosstat18.rds')

###########################################################################################################
########################################### Regression ####################################################
###########################################################################################################

# Empty list where the regression output will be written
# The list is a list of 5 lists - one for each year of data
# 2014, 2015, 2016, 2017, 2018
# and for each year about 85 being the number of regions (H00_02)

Rlm_mincer_all <- vector("list", length(unique(df$YEAR)))
for (i in seq(length(Rlm_mincer_all))){
  Rlm_mincer_all[[i]] <- vector("list", length(unique(df$H00_02)))
}
Rlm_mincer_f = Rlm_mincer_m = Rlm_mincer_all 


# define indices to simplify various loops 
seq_year <- unique(df$YEAR)
df$H00_02 <- as.numeric(as.character(df$H00_02))
seq_region <- unique(df$H00_02) %>% sort()

# Looping over each year and region
# First loop is over year - and then regions within year 
# and skip for regions 35 and 67 in year 2014
# Then the algorithm starts filling in each i,j list
# We are running 85*5 = 425-2 = 423 regressions
# data we have to choose rows - YEAR from i & region from j, weights 
# is the column KVZV ; print(i) is just to display the loop is working

# All
# takes ~ 10 sec
for(i in seq(length(seq_year))){
  for(j in seq(length(seq_region))){
    # Accounting for the absence of data in 2014 for Crimea and Sevastopol
    if(!((j == which(seq_region == 35)| 
          j == which(seq_region == 67)) 
         & i == 1)){
      Rlm_mincer_all[[i]][[j]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                                   data = df[(df$YEAR == seq_year[i] &
                                            df$H00_02 == seq_region[j]),],
                                   weights = df[(df$YEAR == seq_year[i] &
                                                 df$H00_02 == seq_region[j]), "KVZV"])    
    }
  }
  print(i)
}

# By gender
for(i in seq(length(seq_year))){
  for(j in seq(length(seq_region))){
    # Accounting for the absence of data in 2014 for Crimea and Sevastopol
    if(!((j == which(seq_region == 35)|
          j == which(seq_region == 67)) 
         & i == 1)){
      Rlm_mincer_f[[i]][[j]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                                   data = df[(df$YEAR == seq_year[i] &
                                              df$H00_02 == seq_region[j]) & 
                                              df$female == 1,],
                                   weights = df[(df$YEAR == seq_year[i] &
                                                 df$H00_02 == seq_region[j]) & 
                                                 df$female == 1, "KVZV"])   
      Rlm_mincer_m[[i]][[j]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                                   data = df[(df$YEAR == seq_year[i] &
                                              df$H00_02 == seq_region[j]) & 
                                              df$female == 0,],
                                   weights = df[(df$YEAR == seq_year[i] &
                                                 df$H00_02 == seq_region[j]) & 
                                                 df$female == 0, "KVZV"])
    }
  }
  print(i)
}

# Naming the list for each year

names(Rlm_mincer_all) <- seq_year
names(Rlm_mincer_m) <- seq_year
names(Rlm_mincer_f) <- seq_year


# We now run the regression for the whole country, without reference to region
# five sets of regressions
##################################### FOR THE WHOLE SAMPLE ###############################
# Empty list where the regression output will be written
lm_mincer_all <- vector("list", length(unique(df$YEAR)))
for (i in seq(length(lm_mincer_all))){
  lm_mincer_all[[i]] <- vector("list", length(unique(df$H00_02)))
}
lm_mincer_f = lm_mincer_m  = lm_mincer_all 
seq_year <- unique(df$YEAR)



# Looping over each year
for(i in seq(length(seq_year))){
  lm_mincer_all[[i]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                           data = df[df$YEAR == seq_year[i],],
                           weights = df[df$YEAR == seq_year[i], "KVZV"])
                    
  lm_mincer_f[[i]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                         data = df[df$YEAR == seq_year[i] & df$female == 1,],
                         weights = df[df$YEAR == seq_year[i]& df$female == 1, "KVZV"])
  
  lm_mincer_m[[i]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                         data = df[df$YEAR == seq_year[i] &  df$female == 0,],
                         weights = df[df$YEAR == seq_year[i]& df$female == 0, "KVZV"]) 
  
  print(i)
}
names(lm_mincer_all) <- seq_year
names(lm_mincer_f) <- seq_year
names(lm_mincer_m) <- seq_year

# Joining total results with the regression results by regions


for (i in 1:length(Rlm_mincer_all)){
  Rlm_mincer_all[[i]][[length(Rlm_mincer_all[[i]]) + 1]] <- lm_mincer_all[[i]]
  Rlm_mincer_f[[i]][[length(Rlm_mincer_f[[i]]) + 1]] <- lm_mincer_f[[i]]
  Rlm_mincer_m[[i]][[length(Rlm_mincer_m[[i]]) + 1]] <- lm_mincer_m[[i]]
}
############################################################################################

# A file with region names
Sys.setlocale("LC_CTYPE", "russian")
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3"
setwd(wd)
rgvars <- rio::import("rgvars.xlsx") %>% arrange(OKATO)

# Note the strict correspondence necessary between OKATO and H00_02

# Naming sublists with regression summary
for (i in seq(length(seq_year))){
  names(Rlm_mincer_all[[i]]) <- c(rgvars[,3], "Russian Federation")
  names(Rlm_mincer_m[[i]]) <- c(rgvars[,3], "Russian Federation")
  names(Rlm_mincer_f[[i]]) <- c(rgvars[,3], "Russian Federation")
}

# Summarizing
Rsmry_all <- lapply(Rlm_mincer_all, function(x) {lapply(x, summary)})
Rsmry_f <- lapply(Rlm_mincer_m, function(x) {lapply(x, summary)})
Rsmry_m <- lapply(Rlm_mincer_f, function(x) {lapply(x, summary)})





