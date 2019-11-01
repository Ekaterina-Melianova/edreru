# mincer2a.R
# A file to generate the rest of the variables for the mincer equation based on RLMS and run the equation.

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

############################################################################################################

# Working directory
wd <- paste0(normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"), "/Desktop")
###+++++++++++++++++++++SPSPS+++++++++
# Above code does not work for me, we need to find a way to
# work for any user, at least for MS-Windows set up
wd <- "C:/Country/Russia/Data/SEABYTE/RLMS/sqlite"
setwd(wd)



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

# Selecting the variables of interest
df_ <- selectFromSQL(c("AGE", "J13_2", "J10", "J40", "EDUC", "J1",
                       "J5A", "J5B", "H7_2", "H5",
                       "J23", "I2", "I4", "YEAR", "J40", "J35_2Y", "J35_2M",
                       "total_exper", "exper_main_", "exper_add_",
                       "J5A_", "J5B_", "J35_2Y_", "J35_2M_"))

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
    else if (is.factor(df[,i]) == T){
      na_values(df[,i]) <- NULL
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

############################################################################################################

# Filtering age
Freq(df_$AGE)
df <- df_[df_$AGE >= 25 & df_$AGE < 65,]

# Filtering employed
df <- df[df$J1 >= 1 & df$J1 < 5,]
Freq(df$J1)

# Education 

# 4 categories:
# 0 - lower than secondary
# 1 - secondary 
# 2 - specialized / vocational
# 3 - higher and above

Freq(df$EDUC)
df$EDUC <- as.numeric(df$EDUC)
df$edu_4 <- car::recode(df$EDUC, "0:7=0; 8:9=1; 10:11=2; 12=1; 13=2;
                            14=1; 15:17=2; 18=2; 19:20=3; 21:23=3")
Freq(df$edu_4)
df <- UserMisFix(df) # fixing missings after creating a new variable

# Filtering 3 education levels
df <- df[df$edu_4>0,]
Freq(df$edu_4)

# Education as factor
df$edu_4 <- factor(df$edu_4, levels=c(1,2,3),
                              labels=c("Secondary",
                                       "Vocational",
                                       "Higher"))

# Wage

# Select max wage if there is an additional job
# Question J13_2 is missing in 1994, 1995, 1996 - for those years let us use
# J10 as an approximation (the amount of money earned within the previous 30 days)
df$wage <- ifelse(df$YEAR < 1998, as.numeric(apply(df[,c("J10", "J40")], 1, max, na.rm=T)),
                  as.numeric(apply(df[,c("J13_2", "J40")], 1, max, na.rm=T)))

# There are a few of those whose additional wage is greater than the main one
length(df[which(df$J40 > df$J13_2), "IDIND"]) # for 1998 - 2018
length(df[which(df$J40 > df$J10), "IDIND"]) # for 1994 - 1996

df <- UserMisFix(df) # fixing missings after creating a new variable
tail(Freq(df$wage), n = 7L) # ~ 20k NAs

# Socio-demographics

# non-Russian
Freq(df$I4)
df$non_russ[df$I4 == 1] <- 0
df$non_russ[df$I4 > 1] <- 1
df$non_russ[is.na(df$I4)] <- 1
Freq(df$non_russ)

# Gender
Freq(df$H5)
df$female[df$H5==2] <- 1
df$female[df$H5==1] <- 0
Freq(df$female)

# Generating a final dataset for the analysis

names(df)[which(colnames(df) == "total_exper")] <- "exper"
df_mincer <- df[, c("IDIND", "YEAR", "edu_4", "wage", "exper", "non_russ", "female")]
df_mincer$exper <- as.numeric(df_mincer$exper)
summary(df_mincer)
# df[which(df$wage == 0), "ID_I"]

# Filtering the missings left
df_mincer <- df_mincer %>%
  filter(!is.na(wage) & !is.na(exper) & wage > 0)

# F-test
var.test(wage ~ female, df_mincer, 
         alternative = "two.sided")

########################################### Regression ####################################################

# Empty list where the regression output will be written
lm_mincer_all <- vector("list", length(unique(df_mincer$YEAR)))
lm_mincer_f = lm_mincer_m = lm_mincer_rus = lm_mincer_nrus = lm_mincer_all 

seq_year <- unique(df_mincer$YEAR)

# Looping over each year
# all
for(i in seq(length(seq_year))){
  lm_mincer_all[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + non_russ + female,
                     data = df_mincer[df_mincer$YEAR == seq_year[i],])
}
names(lm_mincer_all) <- vec_year

# by gender
for(i in seq(length(seq_year))){
   lm_mincer_f[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + non_russ,
                       data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                          df_mincer$female == 1,])
   lm_mincer_m[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + non_russ,
                               data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                                  df_mincer$female == 0,])  
}

# by nationality
for(i in seq(length(seq_year))){
  lm_mincer_rus[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + female,
                         data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                            df_mincer$non_russ == 0,])
  lm_mincer_nrus[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + female,
                         data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                            df_mincer$non_russ == 1,])  
}

names(lm_mincer_f) <- vec_year
names(lm_mincer_m) <- vec_year

names(lm_mincer_rus) <- vec_year
names(lm_mincer_nrus) <- vec_year

smry_all <- lapply(lm_mincer_all, summary)
smry_f <- lapply(lm_mincer_f, summary)
smry_m <- lapply(lm_mincer_m, summary)
smry_rus <- lapply(lm_mincer_rus, summary)
smry_nrus <- lapply(lm_mincer_nrus, summary)

smry_all
smry_f
smry_m
smry_rus
smry_nrus

# Calculating returns by year for higher and vocational education

RoREs <- as.data.frame((matrix(ncol = 21, nrow = length(vec_year))))
colnames(RoREs) <-  c("YEAR", "returns_to_HE_all", "p_for_HE_all", "returns_to_VE_all", "p_for_VE_all",
                      "returns_to_HE_f", "p_for_HE_f", "returns_to_VE_f", "p_for_VE_f",
                      "returns_to_HE_m", "p_for_HE_m", "returns_to_VE_m", "p_for_VE_m",
                      "returns_to_HE_rus", "p_for_HE_rus", "returns_to_VE_rus", "p_for_VE_rus",
                      "returns_to_HE_nrus", "p_for_HE_nrus", "returns_to_VE_nrus", "p_for_VE_nrus")

# A function for percentages
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Obtaining the values
for (i in seq(length(vec_year))){
  RoREs[i,] <- c(vec_year[i], (percent(exp(smry_all[[i]]$coefficients[3,1]) - 1)),
                 formatC(smry_all[[i]]$coefficients[3,4], digits = 2),
                 (percent(exp(smry_all[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_all[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent(exp(smry_f[[i]]$coefficients[3,1]) - 1)),
                 formatC(smry_f[[i]]$coefficients[3,4], digits = 2),
                 (percent(exp(smry_f[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_f[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent(exp(smry_m[[i]]$coefficients[3,1]) - 1)),
                 formatC(smry_m[[i]]$coefficients[3,4], digits = 2),
                 (percent(exp(smry_m[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_m[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent(exp(smry_rus[[i]]$coefficients[3,1]) - 1)),
                 formatC(smry_rus[[i]]$coefficients[3,4], digits = 2),
                 (percent(exp(smry_rus[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_rus[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent(exp(smry_nrus[[i]]$coefficients[3,1]) - 1)),
                 formatC(smry_nrus[[i]]$coefficients[3,4], digits = 2),
                 (percent(exp(smry_nrus[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_nrus[[i]]$coefficients[2,4], digits = 2))
}

# RoREs

# Converting to data.table and melting in order to visualize
RoREs <- as.data.table(RoREs)
RoREs_1 <- melt(RoREs, measure=c("returns_to_HE_all", "returns_to_VE_all"))
RoREs_1$value <- as.numeric(substr(RoREs_1$value, 1, nchar(RoREs_1$value)-1))

# Plotting all
p1 <- ggplot(RoREs_1, aes(YEAR, value, group = variable, color = variable)) +
  geom_point(size = 3) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
 ylab("Rate of returns, %") +
 xlab("Year")

############################################################################
par(mfrow = c(1,2))

# The same procedure for females
RoREs_2 <- melt(RoREs, measure=c("returns_to_HE_f", "returns_to_VE_f"))
RoREs_2$value <- as.numeric(substr(RoREs_2$value, 1, nchar(RoREs_2$value)-1))

# Plotting females
p2 <- ggplot(RoREs_2, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")


# The same procedure for males
RoREs_3 <- melt(RoREs, measure=c("returns_to_HE_m", "returns_to_VE_m"))
RoREs_3$value <- as.numeric(substr(RoREs_3$value, 1, nchar(RoREs_3$value)-1))

# Plotting males
p3 <- ggplot(RoREs_3, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F) + 
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")

grid.arrange(p2, p3, nrow=1, ncol=2)
############################################################################

# The same procedure for Russians
RoREs_4 <- melt(RoREs, measure=c("returns_to_HE_rus", "returns_to_VE_rus"))
RoREs_4$value <- as.numeric(substr(RoREs_4$value, 1, nchar(RoREs_4$value)-1))

# Plotting females
p4 <- ggplot(RoREs_4, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F)  +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")


# The same procedure for non-Russians
RoREs_5 <- melt(RoREs, measure=c("returns_to_HE_nrus", "returns_to_VE_nrus"))
RoREs_5$value <- as.numeric(substr(RoREs_5$value, 1, nchar(RoREs_5$value)-1))

# Plotting males
p5 <- ggplot(RoREs_5, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F)  +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")

grid.arrange(p4, p5, nrow=1, ncol=2)
############################################################################

# The same procedure for both genders in HE
RoREs_6<- melt(RoREs, measure=c("returns_to_HE_f", "returns_to_HE_m"))
RoREs_6$value <- as.numeric(substr(RoREs_6$value, 1, nchar(RoREs_6$value)-1))

# Plotting females and males - HE
p6 <- ggplot(RoREs_6, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F)  +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")

# The same procedure for both genders in VE
RoREs_7<- melt(RoREs, measure=c("returns_to_VE_f", "returns_to_VE_m"))
RoREs_7$value <- as.numeric(substr(RoREs_7$value, 1, nchar(RoREs_7$value)-1))

# Plotting females and males - VE
p7 <- ggplot(RoREs_7, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F)  +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")

grid.arrange(p6, p7, nrow=1, ncol=2)
############################################################################

# The same procedure for Russians and non-Russians in HE
RoREs_8<- melt(RoREs, measure=c("returns_to_HE_rus", "returns_to_HE_nrus"))
RoREs_8$value <- as.numeric(substr(RoREs_8$value, 1, nchar(RoREs_8$value)-1))

# Plotting females and males - HE
p8 <- ggplot(RoREs_8, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F)  +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")

# The same procedure for Russians and non-Russians in VE
RoREs_9<- melt(RoREs, measure=c("returns_to_VE_rus", "returns_to_VE_nrus"))
RoREs_9$value <- as.numeric(substr(RoREs_9$value, 1, nchar(RoREs_9$value)-1))

# Plotting females and males - VE
p9 <- ggplot(RoREs_9, aes(YEAR, value, group = variable, color = variable)) +
  geom_smooth(se = F)  +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(-10, 110), breaks = seq(-50, 110, 10)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("Rate of returns, %") +
  xlab("Year")
p9

grid.arrange(p8, p9, nrow=1, ncol=2)
###################################################################################################

# Tagging instances (tag1) and unique respondents (tag2)

# If a month of the start of work is missing but a year is not, let us use 1 (January) as an approximation
df[is.na(df$J5B)&is.na(df$J5A)==F, "J5B"] <- 1 # for a main work
df[is.na(df$J35_2M)&is.na(df$J35_2Y)==F, "J35_2M"] <- 1 # for an additional work

df$tag1 <- ifelse(df$exper_main_ > df$exper_add_ & 
                    is.na(df$exper_main_) == F & 
                    is.na(df$exper_add_) == F &
                    df$J5A == df$J5A_ &
                    df$J5B == df$J5B_, 1, 
                  ifelse(df$exper_main_ < df$exper_add_ & 
                           is.na(df$exper_main_) == F & 
                           is.na(df$exper_add_) == F &
                           df$J35_2Y == df$J35_2Y_ & 
                           df$J35_2M == df$J35_2M_, 1, 
                         ifelse(is.na(df$exper_main_) & 
                                is.na(df$exper_add_) == F &
                                df$J35_2Y == df$J35_2Y_ & 
                                df$J35_2M == df$J35_2M_, 1,
                              ifelse(is.na(df$exper_add_) & 
                                     is.na(df$exper_main_) == F &
                                     df$J5A == df$J5A_ &
                                     df$J5B == df$J5B_, 1, 
                                   ifelse(is.na(df$exper_main_) == T 
                                              & is.na(df$exper_add_) == T, -1, 0)))))

table(df$tag1) # 51850 instances with inconsistencies, 77903 - without
# sum(table(df$tag1))

resp <- df %>%
  group_by(IDIND) %>%
  summarise(tag2 = ifelse(any(tag1 == 0), 0, 1))

table(resp$tag2) # 13670 respondents with inconsistencies, 12674 - without

#########################################################################################################

# Generating the initial experience (without inconsistencies fixing)
# Note: safe is a df from mincer1a.R file
safe <- readRDS("safe.rds")
df_ini <- safe
# A function to do that
TotalExper_ini <- function(df, year = "YEAR", ID = "IDIND", int_m = "H7_2", main_y_ = "J5A",
                           main_m_ = "J5B", add_y_ = "J35_2Y", add_m_ = "J35_2M", status = "J1"){
  
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

# We need a list splitted by IDIND
list_ini <- split(df_ini, f = df_ini$IDIND) 

# Appending TotalExper function to this list
list3lap_ini <- pblapply(list_ini, TotalExper_ini) # takes ~ 3 min

# Converting the list back to a dataframe
df_ini <- do.call(rbind.data.frame, list3lap_ini) # takes ~ 2 min

# Naming
names(df_ini)[which(colnames(df_ini) == "total_exper")] <- "exper_ini"

#########################################################################################################

# A table with instancies

# Merging experience computed on the basis of initial variables
df_t <- df %>%
  left_join(df_ini[, c("IDIND", "ID_W", "exper_ini")], by = c("IDIND", "ID_W"))
df_t <- df_t[-which(is.na(df_t$exper)),]

# Formatting
df_t$exper <- as.numeric(df_t$exper)
df_t$exper_ini <- as.numeric(df_t$exper_ini)

# rlms_tbl0: means are aggregated by unique IDIND
rlms_tbl0 <- df_t  %>%
  group_by(IDIND) %>%
  summarise(N = n(),
            exp_mean_ = mean(exper, na.rm = T),
            exp_mean_ini_ = mean(exper_ini, na.rm = T)) 

# rlms_tbl: means are aggregated by the number of instances
rlms_tbl <- rlms_tbl0 %>%
  group_by(N) %>%
  summarise(N_resp = n(),
            exp_mean = mean(exp_mean_, na.rm = T),
            exp_mean_ini = mean(exp_mean_ini_, na.rm = T))

# Checking the distribution  
rlms_tbl$mult <- rlms_tbl$N_*rlms_tbl$N  
sum(rlms_tbl$mult) # ok  

# A list to which a t-test will be applied
list_splt_by_N <- split(rlms_tbl0, f = rlms_tbl0$N)

# Conducting a t-test
t.test.pval <- c() 
for (i in 1:length(list_splt_by_N)){
  base <- list_splt_by_N[[i]][['exp_mean_']]
  against <- list_splt_by_N[[i]][['exp_mean_ini_']]
  test <- t.test(base, against)
  t.test.pval[i] <- round(test$p.value,3)
}  
cbind.data.frame(rlms_tbl$N, t.test.pval)

# Resulting table
rlms_tbl_res <- cbind.data.frame(rlms_tbl, t.test.pval)





















