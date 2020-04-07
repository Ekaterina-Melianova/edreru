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
library(rio)
library(stargazer)
library(xtable)

############################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 

# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Connecting with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")

############################################################################################################

# Selecting the variables of interest
df_ <- selectFromSQL(c("REGION", "AGE", "J13_2", "J10", "J40", "EDUC", "J1",
                       "J5A", "J5B", "H7_2", "H5", "J2COD08",
                       "J23", "I2", "I4", "YEAR", "J40", "J35_2Y", "J35_2M",
                       "total_exper", "exper_main_", "exper_add_",
                       "J5A_", "J5B_", "J35_2Y_", "J35_2M_", 
                       "EDUC"))

dbDisconnect(db)
# Fixing system and user-defined missings in the RLMS database

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

# Naive experience
df$edu_yrs <- car::recode(df$EDUC, "8=8; 9=9; 10=10; 11=11; 12=10;
                              13=10; 14=11; 15=11; 16=12; 17=11; 18=12;
                              19=13; 20=14; 21=16; 22=17; 23=18")
df <- df %>%
  filter(is.na(edu_yrs) == F) # dropping user-defined missings
Freq(df$edu_yrs)
df$exper <- df$AGE - df$edu_yrs - 6
summary(df$exper)

# Generating a final dataset for the analysis
df_mincer <- df[, c("REGION", "IDIND", "YEAR", "edu_4", "wage",
                    "exper", "non_russ", "female",
                    "edu_yrs")]
summary(df_mincer)
# df[which(df$wage == 0), "ID_I"]

# Filtering the missings left
df_mincer <- df_mincer %>%
  filter(!is.na(wage) & !is.na(exper) & wage > 0)

################################## Data with occupation to save ###########################################

# Occupation
Freq(df$J2COD08) # user 407 NAs
df$occup <- as.numeric(df$J2COD08)
df <- df %>% filter(!((occup == 99999997)|
                       (occup == 99999998)|
                       (occup == 99999999)))
Freq(df$occup) 

df_mincer_save <- df[, c("IDIND", "YEAR", "edu_4", "wage",
                    "exper", "non_russ", "female", "occup",
                    "edu_yrs")]
# Filtering the missings left
df_mincer_save <- df_mincer_save %>%
  filter(!is.na(wage) & !is.na(exper) & wage > 0)
summary(df_mincer_save)

# Saving the mincer database for the extension1.R
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)
saveRDS(df_mincer_save, paste0(wd, "/", "df_mincer.rds"))

########################################### Regression ####################################################

# Empty list where the regression output will be written
lm_mincer_all_1 <- vector("list", length(unique(df_mincer$YEAR)))
lm_mincer_f_1 = lm_mincer_m_1 = lm_mincer_all_2 = lm_mincer_f_2 = lm_mincer_m_2 = lm_mincer_all_1

seq_year <- unique(df_mincer$YEAR)

# Looping over each year (all and by gender)

for(i in seq(length(seq_year))){
   # metric education - edu_yrs
   lm_mincer_all_1[[i]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                           data = df_mincer[df_mincer$YEAR == seq_year[i],])
  
   lm_mincer_f_1[[i]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                       data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                          df_mincer$female == 1,])
   
   lm_mincer_m_1[[i]] <- lm(log(wage) ~ edu_yrs + exper + I(exper^2),
                               data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                                  df_mincer$female == 0,])  
   # categorical education - edu_4
   
   lm_mincer_all_2[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                            data = df_mincer[df_mincer$YEAR == seq_year[i],])
   
   lm_mincer_f_2[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                          data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                             df_mincer$female == 1,])
   
   lm_mincer_m_2[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                          data = df_mincer[df_mincer$YEAR == seq_year[i] & 
                                             df_mincer$female == 0,]) 
}

names(lm_mincer_f_1) <- seq_year
names(lm_mincer_m_1) <- seq_year
names(lm_mincer_all_1) <- seq_year
names(lm_mincer_f_2) <- seq_year
names(lm_mincer_m_2) <- seq_year
names(lm_mincer_all_2) <- seq_year

smry_all_1 <- lapply(lm_mincer_all_1, summary)
smry_f_1 <- lapply(lm_mincer_f_1, summary)
smry_m_1 <- lapply(lm_mincer_m_1, summary)
smry_all_2 <- lapply(lm_mincer_all_2, summary)
smry_f_2 <- lapply(lm_mincer_f_2, summary)
smry_m_2 <- lapply(lm_mincer_m_2, summary)

# Calculating returns by year for higher and vocational education

RoREs <- as.data.frame((matrix(ncol = 19, nrow = length(seq_year))))
colnames(RoREs) <-  c("YEAR",
                      "returns_to_edu_all", "p_for_edu_all",
                      "returns_to_edu_f", "p_for_edu_f",
                      "returns_to_edu_m", "p_for_edu_m", 
                      
                      "returns_to_HE_all", "p_for_HE_all", "returns_to_VE_all", "p_for_VE_all",
                      "returns_to_HE_f", "p_for_HE_f", "returns_to_VE_f", "p_for_VE_f",
                      "returns_to_HE_m", "p_for_HE_m", "returns_to_VE_m", "p_for_VE_m")

# A function for percentages
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Obtaining the values
for (i in seq(length(seq_year))){
  RoREs[i,] <- c(seq_year[i],
                 
                 (percent(exp(smry_all_1[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_all_1[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent(exp(smry_f_1[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_f_1[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent(exp(smry_m_1[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_m_1[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent((exp(smry_all_2[[i]]$coefficients[3,1]) - 1)/4)),
                 formatC(smry_all_2[[i]]$coefficients[3,4], digits = 2),
                 (percent((exp(smry_all_2[[i]]$coefficients[2,1]) - 1)/3)),
                 formatC(smry_all_2[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent((exp(smry_f_2[[i]]$coefficients[3,1]) - 1)/4)),
                 formatC(smry_f_2[[i]]$coefficients[3,4], digits = 2),
                 (percent((exp(smry_f_2[[i]]$coefficients[2,1]) - 1)/3)),
                 formatC(smry_f_2[[i]]$coefficients[2,4], digits = 2),
                 
                 (percent((exp(smry_m_2[[i]]$coefficients[3,1]) - 1)/4)),
                 formatC(smry_m_2[[i]]$coefficients[3,4], digits = 2),
                 (percent((exp(smry_m_2[[i]]$coefficients[2,1]) - 1)/3)),
                 formatC(smry_m_2[[i]]$coefficients[2,4], digits = 2))
}

# RoREs
x_axis <- c(c(1994, 1996), seq(2000, 2018, 2))
# Converting to data.table and melting in order to visualize
RoREs <- as.data.table(RoREs)
RoREs_edu <- melt(RoREs, measure=c("returns_to_edu_all", 
                                 "returns_to_edu_f",
                                 "returns_to_edu_m"))
RoREs_edu$value <- as.numeric(substr(RoREs_edu$value, 1, nchar(RoREs_edu$value)-1))
RoREs_edu$variable <- factor(RoREs_edu$variable,
                           labels = c("Total",
                                      "Females",
                                      "Males"))

# Plotting all
ggplot(RoREs_edu, aes(YEAR, value, group = variable, color = variable,
                    shape = variable)) +
  geom_point(aes(shape = variable), size = 4) +
  geom_smooth(se = F, method = 'loess') +
  geom_line() +
  scale_y_continuous(limits = c(5, 15)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key = element_rect(size = 16)) +
  scale_color_manual(values = c("darkgray", "red3", "darkgreen")) +
  #scale_shape_manual(values=c(2,4)) +
  scale_x_discrete(breaks = x_axis) +
  ylab("Rate of returns, %") +
  xlab("Year")

wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)
# Saving
ggsave("re_edu.png", width = 10, height = 7,
       units = "in")

#####################################################################################
RoREs_HE_VE_all <- melt(RoREs, measure=c("returns_to_HE_all", "returns_to_VE_all"))
RoREs_HE_VE_all$value <- as.numeric(substr(RoREs_HE_VE_all$value, 1,
                                           nchar(RoREs_HE_VE_all$value)-1))
RoREs_HE_VE_all$variable <- factor(RoREs_HE_VE_all$variable,
                           labels = c("Higher education",
                                      "Vocational education"))

# Plotting all
ggplot(RoREs_HE_VE_all, aes(YEAR, value, group = variable, color = variable,
                          shape = variable)) +
  geom_point(aes(shape = variable), size = 4) +
  geom_smooth(se = F, method = 'loess') +
  geom_line() +
  scale_y_continuous(limits = c(-1, 30), breaks = seq(0, 30, 2)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key = element_rect(size = 16)) +
  scale_color_manual(values = c("darkgreen", "red")) +
  #scale_shape_manual(values=c(2,4)) +
  scale_x_discrete(breaks = x_axis) +
  ylab("Rate of returns, %") +
  xlab("Year")

# Saving
ggsave("re_HE_all.png", width = 7, height = 7,
       units = "in")

############################################################################
# The same procedure for females
RoREs_HE_VE_f <- melt(RoREs, measure=c("returns_to_HE_f",
                                       "returns_to_VE_f"))
RoREs_HE_VE_f$value <- as.numeric(substr(RoREs_HE_VE_f$value, 1,
                                         nchar(RoREs_HE_VE_f$value)-1))
RoREs_HE_VE_f$variable <- factor(RoREs_HE_VE_f$variable,
                            labels = c("Higher education",
                                       "Vocational education"))

# Plotting females
ggplot(RoREs_HE_VE_f, aes(YEAR, value, group = variable, color = variable)) +
  geom_point(aes(shape = variable), size = 4) +
  geom_smooth(se = F, method = 'loess') +
  geom_line() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 2)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key = element_rect(size = 16)) +
  scale_color_manual(values = c("darkgreen", "red")) +
  scale_x_discrete(breaks = x_axis) +
  ylab("Rate of returns, %") +
  xlab("Year")

# Saving
ggsave("re_HE_f.png", width = 7, height = 7,
       units = "in")

# The same procedure for males
RoREs_HE_VE_m <- melt(RoREs, measure=c("returns_to_HE_m",
                                  "returns_to_VE_m"))
RoREs_HE_VE_m$value <- as.numeric(substr(RoREs_HE_VE_m$value, 1,
                                    nchar(RoREs_HE_VE_m$value)-1))
RoREs_HE_VE_m$variable <- factor(RoREs_HE_VE_m$variable,
                            labels = c("Higher education",
                                       "Vocational education"))
# Plotting males
ggplot(RoREs_HE_VE_m, aes(YEAR, value, group = variable, color = variable)) +
  geom_point(aes(shape = variable), size = 4) +
  geom_smooth(se = F, method = 'loess') +
  geom_line() +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 2)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key = element_rect(size = 16)) +
  scale_color_manual(values = c("darkgreen", "red")) +
  scale_x_discrete(breaks = x_axis) +
  ylab("Rate of returns, %") +
  xlab("Year")

# Saving
ggsave("re_HE_m.png", width = 7, height = 7,
       units = "in")


###################################
# TeX tables

for (i in 1:4){
  cat("\n\\begin{landscape}\n")
  cat("\n\\fontsize{9}{11}\n\\selectfont\n")
  stargazer(lm_mincer_all_1[i],
            lm_mincer_m_1[i],
            lm_mincer_f_1[i],
            lm_mincer_all_2[i],
            lm_mincer_m_2[i],
            lm_mincer_f_2[i],
            type = "latex",
            column.labels = c("Total",
                              "Males", 
                              "Females", 
                              "Total",
                              "Males",
                              "Females"),
            covariate.labels = c("Constant",
                                 "Education, years",
                                 "Vocational education",
                                 "Higher education",
                                 "Experience",
                                 "Experience squared"),
            title = paste0("Results of Mincer Analysis, RLMS ",
                           as.character(seq_year[i])),
            dep.var.caption = "",
            dep.var.labels.include = F,
            df = F,
            ci = T,
            intercept.bottom = F,
            header = F)
  cat("\n\\end{landscape}\n")
  cat("\n\\newpage\n")
}

for (i in 5:length(seq_year)){
  cat("\n\\begin{landscape}\n")
  cat("\n\\fontsize{9}{11}\n\\selectfont\n")
  stargazer(lm_mincer_all_1[i],
            lm_mincer_m_1[i],
            lm_mincer_f_1[i],
            lm_mincer_all_2[i],
            lm_mincer_m_2[i],
            lm_mincer_f_2[i],
            type = "latex",
            column.labels = c("Total",
                              "Males", 
                              "Females", 
                              "Total",
                              "Males",
                              "Females"),
            covariate.labels = c("Constant",
                                 "Education, years",
                                 "Vocational education",
                                 "Higher education",
                                 "Experience",
                                 "Experience squared"),
            title = paste0("Results of Mincer Analysis, RLMS ",
                           as.character(seq_year[i])),
            dep.var.caption = "",
            dep.var.labels.include = F,
            df = F,
            ci = T,
            intercept.bottom = F,
            header = F)
  cat("\n\\end{landscape}\n")
  cat("\n\\newpage\n")
}


# For descriptive statistics
sapply(df_mincer, class)
df_mincer$REGION <- as.numeric(df_mincer$REGION) 
df_mincer$exper <- as.numeric(df_mincer$REGION) 
df_mincer$edu_yrs <- as.numeric(df_mincer$REGION) 
haven::write_sav(df_mincer, "df_mincer.sav")

##############################################################################
##############################################################################
##############################################################################
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
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)
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





















