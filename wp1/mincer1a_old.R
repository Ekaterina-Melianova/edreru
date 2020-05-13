# mincer1a.R
# A file to generate working experience variable in the RLMS database for the mincer equation.

library(foreign)
library(plyr); library(dplyr)
library(stargazer)
library(gmodels)
library(lmtest)
library(sqldf)
library(XLConnectJars)
library(questionr)
library(labelled)
library(tidyr)
library(magrittr)
library(pbapply)

# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

############################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 

# Connecting with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")

############################################################################################################

# Selecting the variables of interest
df_ <- selectFromSQL(c("J1", "AGE", "J5A","J5B","H7_2", "YEAR", "J35_2Y", "J35_2M", 
                       "J13_2", "J10", "J40", "EDUC", "H5", "J2COD08", "J23", "I2", "I4", "J40"))

dbDisconnect(db) # Don't need the sqlite connection anymore
# Fixing system and user-defined missings in the RLMS database

df_ <- SysMisFix(df_) # determining system missings
df_ <- UserMisFix(df_) # labelling user-defined missings

# Zap haven_label introduced by SysMisFix/userMisFix functions - EM perhaps introduce at end of those functions
df_ <- haven::zap_labels(df_)   # Note, some apparently numeric variables have class alphanumeric, eg. J10 monthly wage
############################################## EXPERIENCE #################################################

df_ <- remove_user_na(df_) # Temporarily undo labelling of user-defined missings 


# Dropping system missings on job questions (they were not asked)
df_ <- df_[-which(is.na(df_$J1) == T),]
FreqEM(df_$J1)

# Transforming user-defined missings to NA
df_ <- UserMisFix(df_)
df <- df_ %>%
  set_na_values(99999997:99999999) %>%
  user_na_to_na()
FreqEM(df$J5A)

# Dealing with missings for H7_2 - interview month
FreqEM(df$H7_2)

# Checking years with missings for interview month
(vec <- unique(df[which(is.na(df$H7_2)), "YEAR"])) # an easy way to examine data by year
# Shows that H7_2 or interview month missing for some observations in 1994 and 1995 rounds

sum(is.na(df$H7_2)) # or 
length(df[which(is.na(df$H7_2)), "YEAR"]) 

# H7_2 is missing for 47 observations - 43 for 1994 and 4 for 1995, as can be seen by observation

(df[which(is.na(df$H7_2)),c("YEAR","H7_2")])

# These 47 observations may not be frightfully important, but these lines of code demonstrate the
# method to be used in similar cases

# A table with values for imputation: mean interview month for a year
imths_means <- df %>%
  group_by(YEAR) %>%
  summarize(imths_mean = mean(H7_2, na.rm = T)) %>%
  filter(YEAR %in% vec)

imths_means  # shows the two mean values to be imputed - 11.2 and 10.2

# Imputing the calculated means  for each of the two rows in the dataframe imths_means, indexed by i 
#df[condition and index of year  ] <- newvalue

for (i in 1:nrow(imths_means)){
  df[is.na(df$H7_2) & df$YEAR == as.numeric(imths_means[i,"YEAR"]), "H7_2"] <- round(as.numeric(imths_means[i,"imths_mean"]),1)
}

# check the answer - in addition to the entered numbers in the data, there are 47 new values ! 
table(df$H7_2)

# Filtering missings for the questions about work experience (main and additional)

# If a month of the start of work is missing but a year is not, let us use 1 (January) as an approximation
df[is.na(df$J5B)&is.na(df$J5A)==F, "J5B"] <- 1 # for a main work

df[is.na(df$J35_2M)&is.na(df$J35_2Y)==F, "J35_2M"] <- 1 # for an additional work

# Dealing with user-defined missings

# If a person has a missing value on the start of job date,
# he/she in the predominant majority of cases is NOT employed:

table(df[is.na(df$J5A),"J1"])

# Hence, let us replace those missings with a PREVIOUS non-missing employment starting date

# Converting to a numeric format

df$J5A <- as.numeric(df$J5A)
df$J5B <- as.numeric(df$J5B)
df$J35_2Y <- as.numeric(df$J35_2Y)
df$J35_2M <- as.numeric(df$J35_2M)

# Temporarily excluding those who are currently NOT working (J1==5)
# Further we will merge them to account for the absence of experience when calculating the total one

df_temp1 <-  df %>% 
  arrange(YEAR, IDIND) %>% # making sure the waves are listed sequentially
  filter(J1 < 5)

# Just checking the result
FreqEM(df$J1)
FreqEM(df_temp1$J1)


# The same as my old Ukraine zno and diso trick ! 
# Splitting a dataframe to a list by IDIND
# But Boy, what a huge list this is ! 
list <- split(df_temp1 , f = df_temp1$IDIND) 

# Just manually looking at some observations

list[[1]] #1st individual in the sample IDIND 1, interviewed in 1994 age 21 ; was age 45 in 2018; 
# H5 2 or female; she was still increasing EDUC until age of 27 after which EDUC stabilized at 21 years 
list[[1]][c("YEAR","J10")] # Now what happened to her in 2017 - seems an error, a missing 0?
# and what about 1994 ! Maybe we can make some kind of outlier analysis and imputation? 

list[[31022]] # last individual in sample IDIND 58393, appears once in the sample, a guy 37 years of age with EDUC 16

list[[6578]] # twice, with 7 year gap; in which EDUC declined by 1 !

list[[13453]] # four times, 2003,2004,2006,2007,each year J5A was the same as current year!


temp_ <- as.data.frame(df[is.na(df$J5A)==T & (df$J1 < 5), c("IDIND","REDID_I","J1","YEAR")] )

head(temp_)

list <- split(df_temp1 , f = df_temp1$IDIND) 

df_temp1[df_temp1$IDIND==15982, c("J1","J5A","J5B","AGE","YEAR")]

# Appending a replacement to this list
pb <- txtProgressBar(min = 1, max = length(list), style = 3)

for (i in seq(length(list))){ # !takes ~ 2 min 
  list[[i]] %<>% fill(c(J5A, J5B)) 
  list[[i]] %<>% fill(c(J5A, J5B), .direction = "up")
  # accounts for those who have missing in the first wave but are employed 
  setTxtProgressBar(pb, i)
}


# Converting the list back to a dataframe
df_temp2 <- do.call(rbind.data.frame, list) # takes ~ 1 min
table(df_temp2[is.na(df_temp2$J5A),"J1"]) # Those missings that are left belong to people
# who are employed but have missings in J5A,J5B across all the waves

# 106 such respondents who have missings in both main and additional jobs -> let us drop them
length(df_temp2[is.na(df_temp2$J35_2Y)==T&is.na(df_temp2$J5A),"IDIND"])

# Only 1 person has missing in main job BUT non-missing is additional -> let us keep him
length(df_temp2[is.na(df_temp2$J35_2Y)==F&is.na(df_temp2$J5A),"IDIND"])

# Dropping 106 respondents
df_temp2 <- df_temp2[-which(is.na(df_temp2$J35_2Y)==T&is.na(df_temp2$J5A)),]

# Selecting unemployed 
df_temp3 <- df %>%
  filter(J1==5)

# Merging employed and unemployed
df <- rbind.data.frame(df_temp2, df_temp3)
safe <- df # saving a dataframe at this stage
#df <- safe
#saveRDS(safe,"safe.rds") # saving for further easy retrieval

#########

# Another issue with experience: some people chaotically change the date of their current job across waves.
# Those are mostly older people who perhaps simply don't remember the exact year of start of their job 
# (not talking about month).

# So, in such cases (see below an example for a person with IDIND = 81000901) I replace respondents' date of start
# of a current work (technically I create a new respective variable with underscore _) in each wave 
# with what has been said by them in a PREVIOUS wave.

# An example (if you carefully analyse what this person is responding, you'll find no consistensy in his answers):
df[df$IDIND == 5289, c("IDIND", "ID_I", "ID_W", "J5A", "J5B", "YEAR", "H7_2", "J35_2Y", "J35_2M")]

# To fix the data I generated a function - WorkStartFix - which detects such an inconsistency:
# WHEN A PERSON'S JOB HAS CHANGED BUT ACCOURDING TO HIS/HER ANSWERS IT HAS HAPPENED EARLIER THAN 
# A PREVIOUS INTERVIEW DATE TOOK PLACE.
# If that really was the case, a respondent could have mentioned this job change in the previous (not current) wave.
# To find those cases I created 2 types of time intervals, or distances (in months): between consecutive interview 
# dates (dist_int) and between a current interview date and a date of starting a job according to a current 
# response (dist_int_exp). The replacement needs to be conducted if the former measure is lower than the latter.

# A function to do that
WorkStartFix <- function(df, year = "YEAR", ID = "IDIND", int_m = "H7_2", main_y = "J5A",
                         main_m = "J5B", add_y = "J35_2Y", add_m = "J35_2M", status = "J1"){
  # Converting to numeric format
  df[,main_y] <- as.numeric(df[,main_y])
  df[,main_m] <- as.numeric(df[,main_m])
  df[,add_y] <- as.numeric(df[,add_y])
  df[,add_m] <- as.numeric(df[,add_m])
  df[,int_m] <- remove_labels(df[,int_m])
  
  # Computing work experience
  df[,"imths"] <- (df[,year]*12) + df[,int_m]
  df[,"exper_main"] <- (df[,main_y]*12) + (df[,main_m])
  df[,"exper_add"] <- (df[,add_y]*12) + (df[,add_m])
  
  # Nullifying experience of the unemployed
  df[,"exper_main"] <- ifelse(is.na(df[,main_y])&is.na(df[,add_y]), NA, df[,"exper_main"])
  df[,"exper_add"] <- ifelse(is.na(df[,main_y])&is.na(df[,add_y]), NA, df[,"exper_add"])
  
  # Accounting for negative values (people name a starting year of their work which happened 
  # after the interview date)
  df[,"exper_main"] <- ifelse(df[,"exper_main"] > df[,"imths"], NA, df[,"exper_main"])
  df[,"exper_add"] <- ifelse(df[,"exper_add"] > df[,"imths"], NA, df[,"exper_add"])
  
  df[,"exper_temp"] <- round((df[,"imths"] - apply(df[,c("exper_main", "exper_add")], 1, min, na.rm=T))/12,2)
  df[,"exper_temp"] <- ifelse(is.infinite(df[,"exper_temp"]), 0, df[,"exper_temp"])
  
  # Creating a distance between successive interview dates  
  df <- df %>%
    group_by(IDIND) %>%
    arrange(IDIND, YEAR) %>%
    mutate(dist_int = imths - lag(imths, default = first(imths))) %>%
    ungroup()
  df <- as.data.frame(df)
  
  # Creating dist_int_exp
  df[, "dist_int_exp_main"] <- df[,"imths"] - df[,"exper_main"]
  
  # -Inf values are returned for unemployed since max(NA,NA)=-Inf, replacing those values with 0
  df[, "dist_int_exp_main"] <- ifelse(is.infinite(df[,"dist_int_exp_main"]), 0, df[,"dist_int_exp_main"])
  
  # Creating dist_int_exp
  df[, "dist_int_exp_add"] <- df[,"imths"] - df[,"exper_add"]
  
  # -Inf values are returned for unemployed since max(NA,NA)=-Inf, replacing those values with 0
  df[, "dist_int_exp_add"] <- ifelse(is.infinite(df[,"dist_int_exp_add"]), 0, df[,"dist_int_exp_add"])
  
  
  
  # Splitting a datafrmae to a list
  list2 <- split(df, f = df[,"IDIND"])
  
  # Creating a progress bar, we'll need it in the following loops
  pb <- txtProgressBar(min = 1, max = length(list2), style = 3)
  
  # len is an auxiliary variable, reflecting the length of each subset in list2
  len <- c()
  for (i in seq(length(list2))){
    len[i] <- nrow(list2[[i]])
    setTxtProgressBar(pb, i)
  }
  
  # Assigning the first element (i.e., for the first wave a respondent was surveyed)
  # for each job-related variable of interest in each subset
  for (i in seq(length(list2))){
    list2[[i]][["J5A_"]] <- NA
    list2[[i]][["J5A_"]][[1]] <- list2[[i]][[main_y]][[1]]
    
    list2[[i]][["J5B_"]] <- NA
    list2[[i]][["J5B_"]][[1]] <- list2[[i]][[main_m]][[1]]
    
    list2[[i]][["J35_2Y_"]] <- NA
    list2[[i]][["J35_2Y_"]][[1]] <- list2[[i]][[add_y]][[1]]
    
    list2[[i]][["J35_2M_"]] <- NA
    list2[[i]][["J35_2M_"]][[1]] <- list2[[i]][[add_m]][[1]]
    setTxtProgressBar(pb, i)
  }
  
  # Assigning the rest of the elements in each IDIND-based subsets in the list2
  for (i in seq(length(list2))){
    if (!len[i] == 1){
      for (j in 2:len[i]){
        if (list2[[i]][["dist_int"]][j] < list2[[i]][["dist_int_exp_main"]][j] & 
            is.na(list2[[i]][["dist_int_exp_main"]][j]) == F){
          list2[[i]][["J5A_"]][j] <- list2[[i]][["J5A_"]][j-1]
          list2[[i]][["J5B_"]][j] <- list2[[i]][["J5B_"]][j-1]
        }
        else {
          list2[[i]][["J5A_"]][j] <- list2[[i]][[main_y]][j]
          list2[[i]][["J5B_"]][j] <- list2[[i]][[main_m]][j]
        }
      }
      list2[[i]][["J5A_"]] <- replace(list2[[i]][["J5A_"]], which(is.na(list2[[i]][["J5A_"]]))[1], list2[[i]][["J5A_"]][j])
      list2[[i]][["J5B_"]] <- replace(list2[[i]][["J5B_"]], which(is.na(list2[[i]][["J5B_"]]))[1], list2[[i]][["J5B_"]][j])
      
      setTxtProgressBar(pb, i)
    }
  }
  
  # Assigning the rest of the elements in each IDIND-based subsets in the list2
  for (i in seq(length(list2))){
    if (!len[i] == 1){
      for (j in 2:len[i]){
        if (list2[[i]][["dist_int"]][j] < list2[[i]][["dist_int_exp_add"]][j] & 
            is.na(list2[[i]][["dist_int_exp_add"]][j]) == F){
          list2[[i]][["J35_2Y_"]][j] <- list2[[i]][["J35_2Y_"]][j-1]
          list2[[i]][["J35_2M_"]][j] <- list2[[i]][["J35_2M_"]][j-1]
        }
        else {
          list2[[i]][["J35_2Y_"]][j] <- list2[[i]][[add_y]][j]
          list2[[i]][["J35_2M_"]][j] <- list2[[i]][[add_m]][j]
        }
      }
      list2[[i]][["J35_2Y_"]] <- replace(list2[[i]][["J35_2Y_"]], j, list2[[i]][["J35_2Y_"]][j])
      list2[[i]][["J35_2M_"]] <- replace(list2[[i]][["J35_2M_"]], j, list2[[i]][["J35_2M_"]][j])
      
      setTxtProgressBar(pb, i)
    }
  }
  # Finalizing a dataframe
  fin <- do.call(rbind.data.frame, list2)
  rownames(fin) <- NULL
  
  # Replacing values for the unemployed in the fixed job-related variables by NA
  fin[,"J5A_"] <- ifelse(fin[, status] == 5, NA, fin[,"J5A_"])
  fin[,"J5B_"] <- ifelse(fin[, status] == 5, NA, fin[,"J5B_"])
  fin[,"J35_2Y_"] <- ifelse(fin[, status] == 5, NA, fin[,"J35_2Y_"])
  fin[,"J35_2M_"] <- ifelse(fin[, status] == 5, NA, fin[,"J35_2M_"])
  
  for (i in 1:nrow(fin)){
    fin[i, "J35_2Y_"] <- ifelse(is.na(fin[i, "J35_2Y"]), NA, fin[i, "J35_2Y_"])
    fin[i, "J35_2M_"] <- ifelse(is.na(fin[i, "J35_2M"]), NA, fin[i, "J35_2M_"])
  }
  
  return(fin)
}

# Applying the function to our data: now we have consistent data
# Warnings are ok
df <- WorkStartFix(df) # takes ~ 10 min
summary(df$exper_temp)

# The next step is to compute TOTAL work experience across the RLMS waves

# A function to do that
TotalExper <- function(df, year = "YEAR", ID = "IDIND", int_m = "H7_2", main_y_ = "J5A_",
                       main_m_ = "J5B_", add_y_ = "J35_2Y_", add_m_ = "J35_2M_", status = "J1"){
  
  # Computing work experience with CORRECTED data
  df[,"imths"] <- (df[,year]*12) + df[,int_m]
  df[,"exper_main_"] <- (df[,main_y_]*12) + (df[,main_m_])
  df[,"exper_add_"] <- (df[,add_y_]*12) + (df[,add_m_])
  
  # Accounting for negative values (some people named a starting year of their work 
  # which ostensibly happened after the interview date)
  df[,"exper_main_"] <- ifelse(df[,"exper_main_"] > df[,"imths"], NA, df[,"exper_main_"])
  df[,"exper_add_"] <- ifelse(df[,"exper_add_"] > df[,"imths"], NA, df[,"exper_add_"])
  
  # Computing a variable for the experience in a current job, additional job is accounted for
  df[,"exper_temp_"] <- round((df[,"imths"] - apply(df[,c("exper_main_", "exper_add_")], 1, min, na.rm=T))/12,2)
  
  # -Inf values are returned for unemployed since max(NA,NA)=-Inf, replacing those values with 0
  df[,"exper_temp_"] <- ifelse(is.infinite(df[,"exper_temp_"]), 0, df[,"exper_temp_"])
  
  # Setting the initial value for total experience
  df[1, "total_exper"] <- df[1, "exper_temp_"]
  
  # Creating a distance between successive interview dates
  df <- df %>%
    group_by(IDIND) %>%
    arrange(IDIND, YEAR) %>%
    mutate(dist_int = (imths - lag(imths, default = first(imths)))/12) %>%
    ungroup()
  df <- as.data.frame(df)
  
  # Calculating the final variable. 
  # The logic in this calculation is the following: I take a temporary experience and see 
  # if I need to add the ones in the previous years. I need to do that if and only if 
  # the previous temporary experience falls in the distance between the successive interview
  # dates, meaning that a person gained this experience at a new job. Otherwise, his/her
  # current experience is just a continuation of the previous one so I do not need to add anything.
  
  if (length(df[, "exper_temp_"]) > 1){
    for (i in seq(length(df[, "exper_temp_"]) - 1) ){
      vec1 <- df[1:(i + 1), "dist_int"]
      vec2 <- df[1:(i + 1), "exper_temp_"]
      df[(i + 1), "total_exper"] <- df[(i + 1), "exper_temp_"] + sum(vec2[(which(vec1 >= vec2) - 1)])
    }
    
  }
  return(df)
}

# We need a list splitted by IDIND again
list3 <- split(df, f = df$IDIND) 

# Appending TotalExper function to this list
list3lap <- pblapply(list3, TotalExper) # takes ~ 6 min

# Converting the list back to a dataframe
df <- do.call(rbind.data.frame, list3lap) # takes ~ 2 min

# Let's look at the variable
summary(df$total_exper)

library(ggplot2)
library(ggforce)

ggplot(df, aes(x=total_exper))+ 
  geom_histogram() + 
  facet_wrap_paginate(~as.factor(YEAR), ncol = 3, nrow = 3, page = 1) + 
  theme_bw()

ggplot(df, aes(x=total_exper))+ 
  geom_histogram() + 
  facet_wrap_paginate(~as.factor(YEAR), ncol = 3, nrow = 3, page = 2) + 
  theme_bw()

ggplot(df, aes(x=total_exper))+ 
  geom_histogram() + 
  facet_wrap_paginate(~as.factor(YEAR), ncol = 3, nrow = 3, page = 3) + 
  theme_bw()

# Saving total_exper to a .csv and then to rlms.db
IDIND <- selectFromSQL("IDIND") 

exper_to_csv <- IDIND %>%
  left_join(df[,c("IDIND", "ID_W", "total_exper",
                  "J5A_", "J5B_", "J35_2Y_", "J35_2M_",
                  "exper_main_", "exper_add_", "exper_temp_")],
            by = c("IDIND", "ID_W"))
#write.csv(exper_to_csv, "exper_to_csv.csv")

var_names_to_csv <- c("J5A_", "J5B_", "J35_2Y_", "J35_2M_",
                      "exper_main_", "exper_add_", "exper_temp_",
                      "total_exper")

i = 1
for (name in var_names_to_csv){
  write.csv(exper_to_csv[name], file = paste(wd, "/", name, ".csv", sep=""))
  print(i)
  i = i + 1
}
