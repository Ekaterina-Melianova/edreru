# diebold1a.R
# A file to replicate from RLMS 1998 2008 2017, what Diebold does with CPS 1995 2004 and 2012
# Econometric Data Science Text from Diebold

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

############################################################################################################

# Working directory
wd <- paste0(normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"), "/Desktop")
setwd(wd)
# Connecting with SQLite
db <- dbConnect(SQLite(), dbname=paste0(wd, "/rlms.db"))
# Modified cbind
cbind.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - 
                                                          nrow(x), ncol(x)))))
}
# A function for variable selection
selectFromSQL <- function(column_names=NULL, column_blocks=NULL, wave_number=NULL, dbname = "rlms.db"){
  "
  1. column_names - select specific column/s
  
  2. column_blocks - select specific block/s of columns. Available blocks:
  Bank services
  Children
  Daily activities
  Education
  Elections
  Employment
  Employment/finance (retrospective)
  Family
  Finance
  For women only
  Health assessment
  Identification variables
  Inequity issues
  Insurance
  Interviewer's remarks
  IT skills
  Law
  Living conditions
  Maternal capital
  Medical care
  Migration
  Military service
  Nationality issues
  Other
  Pension
  Personality assessment
  Politics
  Religion
  Safety/crimes
  Shopping
  Socio-demographics
  Sorces of news
  State services
  Transition period
  Traveling
  Trust
  
  3. wave_number - select specific wave/s in RLMS
  
  4. db_name - name of database in SQLite, by default rlms.db
  
  "
  
  # Add blocks to columns
  if (is.null(column_blocks) == FALSE){
    # Load table with blocks
    blocks_df <- sqldf('SELECT * from rlms_blocks', dbname = dbname)
    # Get columns from the selected blocks
    columns_from_blocks <- c()
    for (block in column_blocks){
      columns_from_blocks <- c(columns_from_blocks, blocks_df[blocks_df$column_block == block,]$column_names)
    }
    column_names <- c(column_names, columns_from_blocks)
  }
  
  # Add ids to the columns
  column_names <- unique(c("ID_W", "IDIND", "REDID_I", "ID_I", "ID_H", column_names))
  # Condition on a wave number 
  if (is.null(wave_number) == FALSE){
    if(length(wave_number) == 1){
      wave_condition <- paste('WHERE ID_W =', wave_number)
    } else {
      wave_condition <- paste('WHERE', paste(paste0('ID_W=', wave_number), collapse=' or '))
    }
  } else {
    wave_condition <- ""
  }
  
  # In case if the number of columns is 63 or larger (a default limitation of SQLite)
  if (length(column_names) >= 63){
    # Create a list with column chunks
    column_splits <- split(column_names, ceiling(seq_along(column_names)/63))
    # Add the columns by parts
    result_df <- data.frame()
    for (column_split in column_splits){
      if ('ID_W' %in% column_split == FALSE){
        column_split <- c('ID_W', column_split)
      }
      
      command_line <- paste(c("SELECT", paste(column_split, collapse=', '),
                              "FROM", paste(column_split, collapse=' NATURAL JOIN '),
                              wave_condition), collapse = ' ')
      cat('--- SQL command:', command_line, sep="\n")
      temp_df <- data.frame(sqldf(command_line, dbname = dbname))
      result_df <- data.frame(cbind.all(result_df, temp_df))
    }
    # Remove duplicates of the ID_W column
    result_df <- result_df[, -grep("ID_W.", colnames(result_df))]
    
  } else {
    command_line <- paste(c("SELECT", paste(column_names, collapse=', '),
                            "FROM", paste(column_names, collapse=' NATURAL JOIN '),
                            wave_condition), collapse = ' ')
    cat('--- SQL command:', command_line, sep="\n")
    result_df <- sqldf(command_line, dbname = dbname)
  }
  
  return(result_df)
}

############################################################################################################

# Selecting a subset of variables of interest
df_ <- selectFromSQL(c("ID_I","ID_H","J13_2","EDUC", "J1", "J5A","J5B","H7_2","H5","J23",
                          "I2","I4", "YEAR", "J13_2", "J35_2Y", "J35_2M"))

# Fixing system and user-defined missings in the RLMS database

# Defining functions for a proper treatment of missing values
SysMisFix <- function(df){
  "SysMisFix changes chategorical NA to missing values"
  temp <- df
  for (i in colnames(df)){
    temp[,i] <- mapvalues(df[,i], "NA", NA, warn_missing = F)
    }
  return(temp)
  }
UserMisFix <- function(df, na_range = 99999997:99999999){
  "UserMisFix labels user-defined missings as missing value "
  for (i in colnames(df)){
    if (is.character(df[,i]) == T){
      na_values(df[,i]) <- as.character(na_range)
      }
    else if (!i %in% c("ID_W", "IDIND","YEAR","REDID_I","ID_I","ID_H")){
      na_values(df[,i]) <- na_range
      }
  }
  return(df)
} 

df_ <- SysMisFix(df_) # determining system missings
df_ <- UserMisFix(df_) # labelling user-defined missings

# A function for calculating descriptive statistics: a slightly extended version of freq
Freq <- function(var){
  result <- freq(var, levels = "values", total = T)
  result <- rbind(result, 
                  UserNA = apply(result[as.character(99999997:99999999),],2,sum),
                  TotalNA = apply(result[c(99999997:99999999, "NA"),],2,sum, na.rm = T))
  return(result)
}

# here descriptives of the original data can be computed...
############################################################################################################

# Dropping system missings on work experience (the questions were not asked)
df_ <- remove_user_na(df_) # Temporarily undo labelling of user-defined missings 
df_ <- df_[-which(is.na(df_$J5A) == T),]
Freq(df_$J5A)

#library(compare)
#compare(df_, df__)

# Transforming user-defined missings to NA
df_ <- UserMisFix(df_)
df <- df_ %>%
  set_na_values(99999997:99999999) %>%
  user_na_to_na()
Freq(df$J5A)

# Dealing with missings for H7_2 - int month

# Checking years with missings for int month
(vec <- unique(df[which(is.na(df$H7_2)), "YEAR"]))
length(df[which(is.na(df$H7_2)), "YEAR"]) # H7_2 is missing for 22 observations

# A table with values for imputation: mean interview month for a year
imths_means <- df %>%
  group_by(YEAR) %>%
  summarize(imths_mean = mean(H7_2, na.rm = T)) %>%
  filter(YEAR %in% vec)

# Imputing the calculated means 
for (i in 1:nrow(imths_means)){
  df[is.na(df$H7_2)&df$YEAR == as.numeric(imths_means[i,"YEAR"]), "H7_2"] = round(as.numeric(imths_means[i,"imths_mean"]),1)
}
table(df$H7_2)

# Filtering missings for the questions about work experience (main and additional)

# If a month of the start of work is missing but a year is not, let us use 1 (January) as an approximation
df[is.na(df$J5B)&is.na(df$J5A)==F, "J5B"] <- 1 # for a main work
df[is.na(df$J35_2M)&is.na(df$J35_2Y)==F, "J35_2M"] <- 1 # for an additional work

# Dealing with user-defined missings

# If a person has a missing value on the start of job date,
# he/she in the predominant majority of cases is employed:
table(df[is.na(df$J5A),"J1"])

# Hence, let us replace those missings with a PREVIOUS non-missing employment starting date

# Converting to a numeric format
df$J5A <- as.numeric(df$J5A)
df$J5B <- as.numeric(df$J5B)
df$J35_2Y <- as.numeric(df$J35_2Y)
df$J35_2M <- as.numeric(df$J35_2M)

# Selecting only workers
df_temp1 <-  df %>% 
  arrange(YEAR, ID_I) %>%
  filter(J1==1)

# Splitting a dataframe to a list by ID_I
list <- split(df_temp1 , f = df_temp1$ID_I)

# Appending a replacement to this list
for (i in seq(length(list))){ # ! takes 2 min !
  list[[i]] %<>% fill(c(J5A, J5B, J35_2Y, J35_2M)) 
  list[[i]] %<>%
    fill(c(J5A, J5B, J35_2Y, J35_2M), .direction = "up")# accounts for those who has missing in the first wave
  print(i)
}

# Converting the list back to a dataframe
df_temp2 <- do.call(rbind.data.frame, list) # ! takes 1 min !
table(df_temp2[is.na(df_temp2$J5A),"J1"])

# The missings that are left belong to people appearing in RLMS only once

# 244 such respondents who have missings in both main and additional jobs -> let us drop them
length(df_temp2[is.na(df_temp2$J35_2Y)==T&is.na(df_temp2$J5A),"ID_I"])

# Only 1 person has missing in main job BUT non-missing is additional -> let us keep him
length(df_temp2[is.na(df_temp2$J35_2Y)==F&is.na(df_temp2$J5A),"ID_I"])

# Dropping 244 respondents
df_temp2 <- df_temp2[-which(is.na(df_temp2$J35_2Y)==T&is.na(df_temp2$J5A)),]

# Selecting unemployed 
df_temp3 <- df %>%
  filter(!J1==1)

# Merging employed and unemployed
df <- rbind.data.frame(df_temp2, df_temp3)
safe <- df # saving a dataframe

############################################################################################################

# Another issue with experience: some people chaotically change the date of their current job across waves.
# Those are mostly older people who perhaps simply don't remember the exact year of start of their job 
# (not talking about month).

# So, in such cases (see below an example for a person with ID_I = 81000901) I replace respondents' date of start
# of a current work (technically I create a new respective variable with underscore _) in each wave 
# with what has been said by them in a PREVIOUS wave.

# An example (if you carefully analyse what this person is responding, you'll find no consistensy in his answers):
df[df$ID_I == 81000901, c("ID_I", "ID_W", "J5A", "J5B", "YEAR", "H7_2", "J35_2Y", "J35_2M")]

# To fix the data I generated a function - WorkStartFix - which detects such an inconsistency:
# WHEN A PERSON'S JOB HAS CHANGED BUT ACCOURDING TO HIS/HER ANSWERS IT HAS HAPPENED EARLIER THAN 
# A PREVIOUS INTERVIEW DATE TOOK PLACE.
# If that really was the case, a respondent could have mentioned this job change in the previous (not current) wave.
# To find those cases I created 2 types of time intervals, or distances (in months): between consecutive interview 
# dates (dist_int) and between a current interview date and a date of starting a job according to a current 
# response (dist_int_exp). The replacement needs to be conducted if the former measure is lower than the latter.

# A function to do that
WorkStartFix <- function(df, year = "YEAR", ID = "ID_I", int_m = "H7_2", main_y = "J5A",
                         main_m = "J5B", add_y = "J35_2Y", add_m = "J35_2M"){
  # Converting to numeric format
  df[,main_y] <- as.numeric(df[,main_y])
  df[,main_m] <- as.numeric(df[,main_m])
  df[,add_y] <- as.numeric(df[,add_y])
  df[,add_m] <- as.numeric(df[,add_m])
  
  # Computing work experience
  df[,"imths"] <- (df[,year]*12) + df[,int_m]
  df[,"exper_main"] <- (df[,main_y]*12) + (df[,main_m])
  df[,"exper_add"] <- (df[,add_y]*12) + (df[,add_m])
  df[,"exper_temp"] <- round((df[,"imths"] - apply(df[,c("exper_main", "exper_add")], 1, max, na.rm=T))/12,2)
  #df <- df[is.finite(df$exper_temp),]
  
  df <- df %>%
    group_by(ID_I) %>%
    arrange(ID_I, YEAR) %>%
    mutate(dist_int = imths - lag(imths, default = first(imths)))
  
  df[, "dist_int_exp"] <- df[,"imths"] - apply(df[,c("exper_main", "exper_add")], 1, max, na.rm = T)
  
  list2 <- split(df, f = df[,"ID_I"])
  
  len <- c()
  for (i in seq(length(list2))){
    len[i] <- nrow(list2[[i]]) 
  }
  
  for (i in seq(length(list2))){
    list2[[i]][["J5A_"]] <- NA
    list2[[i]][["J5A_"]][[1]] <- list2[[i]][[main_y]][[1]]
    
    list2[[i]][["J5B_"]] <- NA
    list2[[i]][["J5B_"]][[1]] <- list2[[i]][[main_m]][[1]]
    
    list2[[i]][["J35_2Y_"]] <- NA
    list2[[i]][["J35_2Y_"]][[1]] <- list2[[i]][[add_y]][[1]]
    
    list2[[i]][["J35_2M_"]] <- NA
    list2[[i]][["J35_2M_"]][[1]] <- list2[[i]][[add_m]][[1]]
  }
  for (i in seq(length(list2))){
    if (!len[i] == 1){
      for (j in 2:len[i]){
        if (list2[[i]][["dist_int"]][j] < list2[[i]][["dist_int_exp"]][j]){
          list2[[i]][["J5A_"]][j] <- list2[[i]][["J5A_"]][j-1]
          list2[[i]][["J5B_"]][j] <- list2[[i]][["J5B_"]][j-1]
          list2[[i]][["J35_2Y_"]][j] <- list2[[i]][["J35_2Y_"]][j-1]
          list2[[i]][["J35_2M_"]][j] <- list2[[i]][["J35_2M_"]][j-1]
        }
        else {
          list2[[i]][["J5A_"]][j] <- list2[[i]][[main_y]][j]
          list2[[i]][["J5B_"]][j] <- list2[[i]][[main_m]][j]
          list2[[i]][["J35_2Y_"]][j] <- list2[[i]][[add_y]][j]
          list2[[i]][["J35_2M_"]][j] <- list2[[i]][[add_m]][j]
        }
      }
      list2[[i]][["J5A_"]] <- replace(list2[[i]][["J5A_"]], which(is.na(list2[[i]][["J5A_"]]))[1], list2[[i]][["J5A_"]][j])
      list2[[i]][["J5B_"]] <- replace(list2[[i]][["J5B_"]], which(is.na(list2[[i]][["J5B_"]]))[1], list2[[i]][["J5B_"]][j])
      list2[[i]][["J35_2Y_"]] <- replace(list2[[i]][["J35_2Y_"]], which(is.na(list2[[i]][["J35_2Y_"]]))[1], list2[[i]][["J35_2Y_"]][j])
      list2[[i]][["J35_2M_"]] <- replace(list2[[i]][["J35_2M_"]], which(is.na(list2[[i]][["J35_2M_"]]))[1], list2[[i]][["J35_2M_"]][j])
      
      print(i)
    }
  }
  fin <- do.call(rbind.data.frame, list2)
  rownames(fin) <- NULL
  return(fin)
}

# Applying a function to our data: now we have consistent data
df <- WorkStartFix(df) # takes 5 min

# The nex step is to cumpute a TOTAL work experience across the RLMS waves

# A function to do that
TotalExper <- function(df, year = "YEAR", ID = "ID_I", int_m_ = "H7_2", main_y_ = "J5A_",
                       main_m_ = "J5B_", add_y_ = "J35_2Y_", add_m_ = "J35_2M_", status = "J1"){
  
  # Computing work experience with corrected data
  
  # df[,"imths"] <- (df[,year]*12) + df[,int_m]
  df[,"exper_main_"] <- (df[,main_y_]*12) + (df[,main_m_])
  df[,"exper_add_"] <- (df[,add_y_]*12) + (df[,add_m_])
  #df[,"exper_temp_"] <- ifelse(df[,status]==1, # temporary experience only for those who are employed,
   #                                            # unemployed recieve 0 for a current wave
    #                           round((df[,"imths"] - apply(df[,c("exper_main_",
     #                                                            "exper_add_")], 1, max, na.rm=T))/12,2),0)
  
  df[,"exper_temp_"] <- round((df[,"imths"] - apply(df[,c("exper_main_",
                                                                 "exper_add_")], 1, max, na.rm=T))/12,2)

  df[1, "total_exper"] <- df[1, "exper_temp_"]
  
  is.sequential <- function(x){
    all(diff(x) > 0)
  } 
  
  if (nrow(df[, "exper_temp_"]) > 1){
    for (i in seq((nrow(df[, "exper_temp_"]) - 1))){
      if (df[i + 1, "exper_temp_"] > df[i, "exper_temp_"] &
          is.sequential(df[1:i + 1, "exper_temp_"]) == T){
        df[i + 1, "total_exper"] <- df[i + 1, "exper_temp_"]
      }
      else if (df[i + 1, "exper_temp_"] > df[i, "exper_temp_"] &
               is.sequential(df[1:i + 1, "exper_temp_"]) == F){
        vec <- df[1:i + 1, "exper_temp_"]
        df[i + 1, "total_exper"] <- df[i + 1, "exper_temp_"] + sum(df[which(diff(vec) < 0), "exper_temp_"])
      }
      else if (df[i + 1, "exper_temp_"] < df[i, "exper_temp_"]){
        df[i + 1, "total_exper"] <- df[i + 1, "exper_temp_"] + df[i, "exper_temp_"]
      }
    }
  }
  return(df)
}

# We need a list splitted by ID_I again
list3 <- split(df, f = df$ID_I) # takes ~ 1 min

# Appending TotalExper function to this list
for (i in seq(length(list3))){ # ! takes 5 min !
  list3[[i]] <- TotalExper(list3[[i]])
  print(i)
}

# Converting the list back to a dataframe
df <- do.call(rbind.data.frame, list3)


test <- WorkStartFix(test)
test <- TotalExper(test)






# Fixing bags in total_exper: negative values
Freq_total_exper <- Freq(result_df$total_exper)

# Saving total_exper to a .csv and then to rlms.db
ID_I <- selectFromSQL("ID_I") 
exper_to_csv <- ID_I %>%
  left_join(result_df[,c("ID_I", "ID_W", "total_exper")], by = c("ID_I", "ID_W"))
write.csv(exper_to_csv, "exper_to_csv2.csv", na = "NA")


length(unique(exper_to_csv$ID_I))

# Education variable
df$edu <- factor(df$edu_4_cat,
                              levels=c(1,2,3),
                              labels=c("Secondary",
                                       "Specialized / vocational",
                                       "Higher"))

# Regress LNWAGE ~ FEMALE + NONWHITE + UNION + EDUC + EXPER ---------------

reg.lnwage.by.educ.exper.female.nonr.pubs <- lm(lnwage.actual~EDUC+EXPER+NON_RUSS+FEMALE+PUB_SEC, data=s1b_98)
summary(reg.lnwage.by.educ.exper.female.nonr.pubs)

# RESET test
# No power terms needed p-values very high; low F, cannot 
# reject null of no higher orders needed
resettest(reg.lnwage.by.educ.exper.female.nonr.pubs, power=2, type="fitted")
resettest(reg.lnwage.by.educ.exper.female.nonr.pubs, power=2:3, type="fitted")

## In this case, needed, as F values ver high
resettest(reg.lnwage.by.educ.exper, power=2, type="fitted")
resettest(reg.lnwage.by.educ.exper, power=2:3, type="fitted")

reg3 <- lm(lnwage.actual~EDUC+I(EDUC^2)+EXPER+I(EXPER^2)+
             NON_RUSS+FEMALE+PUB_SEC+EDUC*EXPER+
             FEMALE*PUB_SEC+FEMALE*NON_RUSS+NON_RUSS*PUB_SEC, 
           data=s1b_98)
summary(reg3)

# Not much new to be learnt in this book ! 

