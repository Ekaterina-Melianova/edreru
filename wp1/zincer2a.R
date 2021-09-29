# zincer2a.R
# Working Paper 1

library(foreign)
library(plyr); library(dplyr)
library(gmodels)
library(lmtest)
library(sqldf)
# library(XLConnectJars) Not on Cran as of April 28, 2020
library("XLConnectJars",lib.loc="C:/Users/wb164718/Documents/R/win-library/3.5")
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
library(openxlsx)

############################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 

# Some functions 
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Connecting with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")

############################################################################################################
## WARNING - Depending on how the R environments are set up.
## re-running this command or running this command after dbDisconnect(db)
## and then connecting again - gives cryptic and wrong error message

## Hence, running in one go and saving zdf_  SP May 14, 2020

# below took system.time() about 15 seconds elapsed time
# Selecting the variables of interest
zdf_ <- selectFromSQL(c("REGION", "AGE", "J13_2", "J10", "J40", "EDUC", "J1",
                       "J5A", "J5B", "H7_2", "H5", "J2COD08",
                       "J23", "I2", "I4", "YEAR", "J40", "J35_2Y", "J35_2M",
                       "total_exper", "exper_main_", "exper_add_",
                       "J5A_", "J5B_", "J35_2Y_", "J35_2M_", 
                       "EDUC", 'J72_5C', 'J72_6A', 'J72_4C', 'J72_3C',
                       'J70', 'J70_1', 'J72_2C', 'J72_18A'))


dbDisconnect(db)

#saveRDS(zdf_,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/zdf_.rds")
zdf_ <- readRDS("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/zdf_.rds")

# Fixing system and user-defined missings in the RLMS database

df_ <- zdf_

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
# Running this code after R upgrade July 22 gives error message, labels need to be removed  SP
df_ <- remove_labels(df_, user_na_to_na = TRUE, keep_var_label = FALSE)

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
tail(Freq(df$wage), n = 7L) # ~ 20k NAs exactly 22,427 in run of Jul 23

# Socio-demographics

# Running this code after R upgrade July 22 gives error message, labels need to be removed  SP
df <- remove_labels(df, user_na_to_na = TRUE, keep_var_label = FALSE)

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

df_mincer <- df[, c("REGION", "IDIND", "YEAR", "edu_4", "wage", 'EDUC',
                    "exper", "non_russ", "female",
                    "edu_yrs", 'AGE', 'J72_5C', 'J72_6A', 'J72_4C', 'J72_3C',
                    'J70', 'J70_1', 'J72_2C', 'J72_18A')]
summary(df_mincer)
glimpse(df_mincer)

# Filtering the missings left
df_mincer <- df_mincer %>%  filter(!is.na(wage)) # becomes 115162 from 137,446
df_mincer <- df_mincer %>%  filter(!is.na(exper)) # stays 115162
df_mincer <- df_mincer %>%  filter(wage!=0) # becomes 114149 ie, 1013 lost

                                   

# Filtering 3 education levels
df_mincer <- df_mincer[df_mincer$edu_4>0,] # becomes 113,337
Freq(df_mincer$edu_4)

# Education as factor
df_mincer$edu_4 <- factor(df_mincer$edu_4, levels=c(1,2,3),
                          labels=c("Secondary",
                                   "Vocational",
                                   "Higher"))

################################################ Education revised (VG)
df_mincer <- haven::zap_labels(df_mincer)

# to numeric
df_mincer[, c('J72_5C', 'J72_6A', 'J72_4C', 'J72_3C',
              'J70', 'J70_1', 'J72_2C', 'J72_18A')] <-
  sapply(df_mincer[, c('J72_5C', 'J72_6A', 'J72_4C', 'J72_3C',
                       'J70', 'J70_1', 'J72_2C', 'J72_18A')], as.numeric)

df_mincer$educ_level <- NA
df_mincer$educ_level <- ifelse(df_mincer$J72_5C==1 | df_mincer$J72_6A==1, 7, df_mincer$educ_level)
df_mincer$educ_level <- ifelse(is.na(df_mincer$educ_level) & df_mincer$J72_4C==1, 6, df_mincer$educ_level)
df_mincer$educ_level <- ifelse(is.na(df_mincer$educ_level) & df_mincer$J72_3C==1, 5, df_mincer$educ_level)
df_mincer$educ_level <- ifelse(is.na(df_mincer$educ_level) & (df_mincer$J70 == 1 & 
                                                  df_mincer$J70_1 > 9 & df_mincer$J70_1 < 97), 4, df_mincer$educ_level)
df_mincer$educ_level <- ifelse(is.na(df_mincer$educ_level) & df_mincer$J72_2C==1, 3, df_mincer$educ_level)
df_mincer$educ_level <- ifelse(is.na(df_mincer$educ_level) & df_mincer$J72_18A==1, 2, df_mincer$educ_level)
df_mincer$educ_level <- ifelse(is.na(df_mincer$educ_level) & df_mincer$J72_18A==3, 2, df_mincer$educ_level)

table(df_mincer$educ_level)
summary(df_mincer$educ_level)

# 4 categories for the revised education
df_mincer$educ_level_4 <- ifelse(df_mincer$educ_level == 2, 0,
                          ifelse(df_mincer$educ_level == 4, 1,
                          ifelse(df_mincer$educ_level == 3 |df_mincer$educ_level == 5 |df_mincer$educ_level == 6, 2,
                          ifelse(df_mincer$educ_level == 7, 3, df_mincer$educ_level))))
table(df_mincer$educ_level_4)
table(df_mincer$edu_4, df_mincer$educ_level_4)
table(df_mincer$edu_4, df_mincer$educ_level)


################## Generating a table for vocational education ###############################
# Select Vocational only
df_mincer_voc <- df_mincer %>% filter(edu_4 == 'Vocational' & educ_level %in% c(3,5,6))
table(df_mincer_voc$educ_level)

df_mincer_voc$edu_yrs_9 <- df_mincer_voc$edu_yrs - 9 

# Creating xtab for mean vocational years after 9 years of schooling
mean_yrs_voc <- data.frame(xtabs(edu_yrs_9 ~ YEAR + educ_level,
      aggregate(edu_yrs_9 ~ YEAR + educ_level, df_mincer_voc, mean)))
# Computing totals
mean_yrs_voc <- mean_yrs_voc  %>%
  group_by(YEAR) %>%
  dplyr::mutate(Total_mean = mean(Freq))
names(mean_yrs_voc)[3] <- 'm_yrs'
margins1 <- unique(mean_yrs_voc[, c('YEAR', 'Total_mean')])
names(margins1)[2] <- 'm_yrs'
mean_yrs_voc$Total_mean <- NULL

# Computing counts for a table with vocational years after 9 years of schooling
freq_yrs_voc <- data.frame(xtabs( ~ YEAR + educ_level, df_mincer_voc))
freq_yrs_voc$YEAR <- as.character(freq_yrs_voc$YEAR)
# Computing totals
freq_yrs_voc <- freq_yrs_voc  %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(Total = sum(Freq))
margins2 <- unique(freq_yrs_voc[, c('YEAR', 'Total')])
names(margins2)[2] <- 'Freq'
freq_yrs_voc$Total <- NULL

# Merging all totals
margins <- margins1 %>%
  left_join(margins2, by = 'YEAR')

# Final table
voc_smry <- mean_yrs_voc  %>%
  full_join(freq_yrs_voc, by = c('YEAR', 'educ_level'))
margins$educ_level <- 'Total'
voc_smry <- rbind.data.frame(voc_smry, margins)
voc_smry$m_yrs <- round(voc_smry$m_yrs, 2)
names(voc_smry) <- c('Year', 'VG_level', 'Mean_Edu_Years_after_9', 'N')

# Arranging
hist(voc_smry$Mean_Edu_Years_after_9)

names(voc_smry) <- c("Year","VG_level","Mean_Edu_Years_after_9","N")
#voc_smry <- voc_smry %>% dplyr::arrange(Year)
# Weird error message "YEAR" not found 
voc_smry <- voc_smry[order(voc_smry$Year),]

# Plotting
voc_smry <- voc_smry %>% filter(VG_level %in% c(3,5,6))
ggplot(voc_smry, aes(Year, Mean_Edu_Years_after_9, color = VG_level, group = VG_level)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12)) +
  scale_y_continuous(limits = c(1.8, 3.2)) +
  scale_color_manual(values = c('red', 'darkgreen', 'blue'),
                     labels = c('incomplete secondary + vocational training',
                                'secondary school + vocational training',
                                'college '))+ 
  guides(color=guide_legend(title="Vocational Education Level"))

########################################################################

setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1")
# Earnings Ratio by Educational Level

# Average earnings secondary level
edu_0 <- df_mincer %>%
  group_by(edu_4, YEAR) %>%
  filter(edu_4 == "Secondary" & YEAR %in% c(1998, 2006, 2018)) %>%
  dplyr::summarise(wage_sec = mean(wage))

# Average earnings for all levels
edu_ratio <- df_mincer %>%
  group_by(edu_4, YEAR) %>%
  filter(YEAR %in% c(1998, 2006, 2018))  %>%
  dplyr::summarise(wage_by_level = mean(wage))

# Merging and computing ratios
edu_ratio <- edu_ratio %>%
  left_join(edu_0, by = 'YEAR')
edu_ratio$edu_ratio <- 100*round(edu_ratio$wage_by_level/edu_ratio$wage_sec, 2)

# Plotting for a subset of years
g1 <- ggplot(data=edu_ratio[edu_ratio$YEAR == 1998,], aes(x = edu_4.x, y = edu_ratio,
                                                          fill = edu_4.x)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('grey', 'darkgreen', 'darkred')) +
  geom_text(aes(y = edu_ratio, label = edu_ratio, vjust = -0.5), color="black", size = 5) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('1998')

g2 <- ggplot(data=edu_ratio[edu_ratio$YEAR == 2006,], aes(x = edu_4.x, y = edu_ratio,
                                                          fill = edu_4.x)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('grey', 'darkgreen', 'darkred')) +
  geom_text(aes(y = edu_ratio, label = edu_ratio, vjust = -0.5), color="black", size = 5) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.ticks = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('2006')

g3 <- ggplot(data=edu_ratio[edu_ratio$YEAR == 2018,], aes(x = edu_4.x, y = edu_ratio,
                                                          fill = edu_4.x)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('grey', 'darkgreen', 'darkred')) +
  geom_text(aes(y = edu_ratio, label = edu_ratio, vjust = -0.5), color="black", size = 5) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.ticks = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('2018')

g <- gridExtra::grid.arrange(g1, g2, g3, nrow = 1, ncol = 3)

# saving 
ggsave("earnings_ratio.png", g, width = 12, height = 6,
       units = "in")

################### Age-earning Profiles by Level of Education
############
# Adjusting to prices in 2018
cpi <- rio::import("cpi_revised.xlsx")[, c(1,6)]
df_mincer <- df_mincer %>% left_join(cpi, by="YEAR")
df_mincer2 <- df_mincer %>% 
  mutate(wage_c18=ifelse(YEAR >=1998,wage*cons_wb,(wage*cons_wb/1000)))
df_mincer2 <- haven::zap_labels(df_mincer2)
############

## Now the same plot, arranged with years together

temp_ <- df_mincer2 %>% filter(YEAR==1998) 
g1 <- ggplot(temp_,aes(x=AGE,y=wage_c18,group=as.factor(edu_4),color=as.factor(edu_4))) +
  geom_smooth(aes(linetype = edu_4), se=FALSE, lwd=2, method=loess)+
  scale_linetype_manual(values = c('longdash', 'dotted', 'solid'), guide = 'none') +
  coord_cartesian(ylim=c(5000,40000))+
  scale_color_manual(values=c("#D50B53","#824CA7","#B9C406"),
                     breaks = rev(levels(as.factor(temp_$edu_4))))+
  xlab("Age") + ylab("Monthly wages in 2018 Rubles")+
  theme(panel.background = element_rect(fill = "#eeecec")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "#F4D993"),
        legend.key = element_rect(fill = "#F4D993"),
        legend.title=element_blank(),
        axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'),
        legend.key.size = unit(3,"line"),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('1998') +
  guides(shape = FALSE,
         colour = guide_legend(override.aes = list(color = c("#D50B53","#824CA7","#B9C406"),
                                                   linetype = c('longdash', 'dotted', 'solid'))))

###

temp_ <- df_mincer2 %>% filter(YEAR==2006) 
g2 <- ggplot(temp_,aes(x=AGE,y=wage_c18,group=as.factor(edu_4),color=as.factor(edu_4))) +
  geom_smooth(aes(linetype = edu_4), se=FALSE, lwd=2, method=loess)+
  scale_linetype_manual(values = c('longdash', 'dotted', 'solid'), guide = 'none') +
  coord_cartesian(ylim=c(5000,40000))+
  scale_color_manual(values=c("#D50B53","#824CA7","#B9C406"),
                     breaks = rev(levels(as.factor(temp_$edu_4))))+
  xlab("Age") + 
  theme(panel.background = element_rect(fill = "#eeecec")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.title.y = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('2006')


###

temp_ <- df_mincer2 %>% filter(YEAR==2018) 
g3 <- ggplot(temp_,aes(x=AGE,y=wage_c18,group=as.factor(edu_4),color=as.factor(edu_4))) +
  geom_smooth(aes(linetype = edu_4), se=FALSE, lwd=2, method=loess)+
  scale_linetype_manual(values = c('longdash', 'dotted', 'solid'), guide = 'none') +
  coord_cartesian(ylim=c(5000,40000))+
  scale_color_manual(values=c("#D50B53","#824CA7","#B9C406"),
                     breaks = rev(levels(as.factor(temp_$edu_4))))+
  xlab("Age") + 
  theme(panel.background = element_rect(fill = "#eeecec")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.title.y = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle('2018')

g <- gridExtra::grid.arrange(g1, g2, g3, nrow = 1, ncol = 3)

# saving 
ggsave("earnings_by_level.png", g, width = 20, height = 8,
       units = "in")

########################################################################
####### Descriptive stat
library(tables)
desc_rst <- tabular((Year = factor(YEAR) )~ (N=1) + 
                      Format(digits=2)*((Wage = wage*((Mean = mean) + (SD = sd))) +
                                          (Experience = exper*((Mean = mean) + (SD = sd))) +
                                          (Education_years = edu_yrs*((Mean = mean) + (SD = sd))) +
                                          (Education = factor(edu_4)*Percent("row"))),
                    data = df_mincer)

#desc_rst

Hmisc::latex(desc_rst)

################################## Data with occupation to save ###########################################

# Occupation

tail(Freq(df$J2COD08), n = 7L)  # used to be user 407 NAs; now 403 simple NAs


df$occup <- as.numeric(df$J2COD08)
df <- df %>% filter(!((occup == 99999997)|
                       (occup == 99999998)|
                       (occup == 99999999)))
tail(Freq(df$occup), n = 7L)

df_mincer_save <- df[, c("IDIND", "YEAR", "edu_4", "wage",
                    "exper", "non_russ", "female", "occup",
                    "edu_yrs")]

# Filtering the missings left


df_mincer_save <- df_mincer_save %>%  filter(!is.na(wage)) # less 22,027 so 137043 becomes 115,016
df_mincer_save <- df_mincer_save %>%  filter(!is.na(exper)) # no change
df_mincer_save <- df_mincer_save %>%  filter(wage>0) # less 1,011 so 115,016 becomes 114,005


summary(df_mincer_save)

# Saving the mincer database for the extension1.R
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)
#saveRDS(df_mincer_save, paste0(wd, "/", "df_mincer.rds"))

########################################### Regression ####################################################

df_mincer <- readRDS("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/df_mincer.rds") # from May 17, 2020
# New run for re-write, August 01, 2020
df_mincer <- df_mincer_save


df_mincer_HP <- haven::zap_labels(df_mincer)
str(df_mincer_HP)

haven::write_dta(data=df_mincer_HP, path="df_mincer.dta")


# Empty list where the regression output will be written
# Each list has 23 null items, there are two sets of all, male and female - for edu_year and by category
lm_mincer_all_1 <- vector("list", length(unique(df_mincer$YEAR)))
lm_mincer_f_1 = lm_mincer_m_1 = lm_mincer_all_2 = lm_mincer_f_2 = lm_mincer_m_2 = lm_mincer_all_1

(seq_year <- unique(df_mincer$YEAR))
# [1] 1994 1995 1996 1998 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
# str(seq_year) # is an integer vector with 23 elements


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

# We now have 23 times 6 or 138 sets of regressionn results, stored as a list object ; let's name each of the objects
# by the year

names(lm_mincer_f_1) <- seq_year
names(lm_mincer_m_1) <- seq_year
names(lm_mincer_all_1) <- seq_year
names(lm_mincer_f_2) <- seq_year
names(lm_mincer_m_2) <- seq_year
names(lm_mincer_all_2) <- seq_year

# Now create another list object, which is the set of summary objects of the 138 sets of results

smry_all_1 <- lapply(lm_mincer_all_1, summary)
smry_f_1 <- lapply(lm_mincer_f_1, summary)
smry_m_1 <- lapply(lm_mincer_m_1, summary)
smry_all_2 <- lapply(lm_mincer_all_2, summary)
smry_f_2 <- lapply(lm_mincer_f_2, summary)
smry_m_2 <- lapply(lm_mincer_m_2, summary)

# Calculating returns by year for higher and vocational education

# Create an empty data frame of 19 columns - year (all f m)  (allh fh mh) (allv fv mv) coeffs and p values
RoREs <- as.data.frame((matrix(ncol = 19, nrow = length(seq_year))))
# provide coumn names to the 19 columns 
colnames(RoREs) <-  c("YEAR",
                      "returns_to_edu_all", "p_for_edu_all",
                      "returns_to_edu_f", "p_for_edu_f",
                      "returns_to_edu_m", "p_for_edu_m", 
                      "returns_to_HE_all", "p_for_HE_all", 
                      "returns_to_VE_all", "p_for_VE_all",
                      "returns_to_HE_f", "p_for_HE_f",
                      "returns_to_VE_f", "p_for_VE_f",
                      "returns_to_HE_m", "p_for_HE_m",
                      "returns_to_VE_m", "p_for_VE_m")

# A function for percentages
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}




# Obtaining the values
for (i in seq(length(seq_year))){
  RoREs[i,] <- c(seq_year[i],
                 
                 (percent(exp(smry_all_1[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_all_1[[i]]$coefficients[2,4], digits = 4),
                 
                 (percent(exp(smry_f_1[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_f_1[[i]]$coefficients[2,4], digits = 4),
                 
                 (percent(exp(smry_m_1[[i]]$coefficients[2,1]) - 1)),
                 formatC(smry_m_1[[i]]$coefficients[2,4], digits = 4),
                 
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
(x_axis <- c(c(1994, 1996), seq(2000, 2018, 2)))
# Converting to data.table and melting in order to visualize
RoREs <- as.data.table(RoREs)
RoREs_edu <- melt(RoREs, measure=c("returns_to_edu_all", 
                                 "returns_to_edu_f",
                                 "returns_to_edu_m"))
RoREs_edu$value <- as.numeric(substr(RoREs_edu$value, 1, nchar(RoREs_edu$value)-1))

## 
write.xlsx(RoREs_edu,file="RoREs_edu.xlsx")


RoREs_edu$variable <- factor(RoREs_edu$variable,
                           labels = c("Males",
                                      "Females",
                                      "Total"))
# Factor corrected by SP - earlier version of paper 
# Prior to May 14 showed total in middle which makes sense





## Let's take df_mincer and look at results for Mincerian

# 1994
df_m94 <- df_mincer %>% filter(YEAR==1994)
haven::write_dta(data=df_m94, path="df_mincer94.dta")

lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m94) 
smry_all_1$`1994`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m94[df_m94$female==1,]) 
smry_f_1$`1994`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m94[df_m94$female==0,]) 
smry_m_1$`1994`$coefficients[2,1]


ex_94all <- data.frame(IDIND=df_m94$IDIND,female=df_m94$female,logwage=log(df_m94$wage),edu_yrs=df_m94$edu_yrs,yhat_all=lm_mincer_all_1[[1]]$fitted.values)
ex_94F   <- data.frame(IDIND=df_m94[df_m94$female==1,c("IDIND")],yhat_G=lm_mincer_f_1[[1]]$fitted.values)
ex_94M   <- data.frame(IDIND=df_m94[df_m94$female==0,c("IDIND")],yhat_G=lm_mincer_m_1[[1]]$fitted.values)
ex_94FM  <- rbind(ex_94F,ex_94M) %>% arrange(IDIND)
ex_94AFM <- inner_join(ex_94all,ex_94FM,by="IDIND")


linear = function(k) {
  z <- list(xx = format(coef(k)[1], digits = 4),
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

linear = function(k) {
  z <- list(xx = coef(k)[1],
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

ggplot()+geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black",lty="solid"),se=FALSE,lwd=1,
                     method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=logwage,color="black",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=logwage,color="limegreen",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=logwage,color="blue",lty="dashed"),se=FALSE,lwd=1)+
  scale_color_identity(guide="legend",name= "Colors",
                       breaks=c("black","limegreen","blue"),
                       labels= c("All","Female","Male"))+
  scale_linetype_identity(guide=guide_legend(override.aes = list(col = 'black')),name="Line type",labels=c("Log Wage","Hatted lwage"))+
  annotate("text", x = 5, y = 13, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==0,])), colour="blue", size = 5, parse=TRUE)+
  annotate("text", x = 5, y = 12.8, label = linear(lm(yhat_all~ edu_yrs, data=ex_94AFM)), colour="black", size = 5, parse=TRUE)+
  annotate("text", x = 5, y = 12.6, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==1,])), colour="limegreen", size = 5, parse=TRUE)+
  annotate("text", x = 11, y = 13, 
           label = paste0("[",format(lm_mincer_m_1$`1994`$coefficients[2],digits=4),"]"), colour="blue", size = 5,
           fontface="italic")+
  annotate("text", x = 11, y = 12.8, 
           label = paste0("[",format(lm_mincer_all_1$`1994`$coefficients[2],digits=4),"]"), colour="black", size = 5,
           fontface="italic")+
  annotate("text", x = 11, y = 12.6, 
           label = paste0("[",format(lm_mincer_f_1$`1994`$coefficients[2],digits=4),"]"), colour="limegreen", size = 5,
           fontface="italic") +
 #geom_point(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen"),alpha=0.7)+ 
 #geom_point(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue"))+ 
 #geom_point(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black"))+ 
  ggtitle("Examining Predicted Mincerian Log Wage against Edu_Years for 1994")
ggsave("diag94.png", width = 10, height = 7,units = "in")

  
  
############################################
# 2000
df_m00 <- df_mincer %>% filter(YEAR==2000)
haven::write_dta(data=df_m00, path="df_mincer94.dta")

lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m00) 
smry_all_1$`2000`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m00[df_m00$female==1,]) 
smry_f_1$`2000`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m00[df_m00$female==0,]) 
smry_m_1$`2000`$coefficients[2,1]


ex_94all <- data.frame(IDIND=df_m00$IDIND,female=df_m00$female,logwage=log(df_m00$wage),edu_yrs=df_m00$edu_yrs,yhat_all=lm_mincer_all_1[[5]]$fitted.values)
ex_94F   <- data.frame(IDIND=df_m00[df_m00$female==1,c("IDIND")],yhat_G=lm_mincer_f_1[[5]]$fitted.values)
ex_94M   <- data.frame(IDIND=df_m00[df_m00$female==0,c("IDIND")],yhat_G=lm_mincer_m_1[[5]]$fitted.values)
ex_94FM  <- rbind(ex_94F,ex_94M) %>% arrange(IDIND)
ex_94AFM <- inner_join(ex_94all,ex_94FM,by="IDIND")


linear = function(k) {
  z <- list(xx = format(coef(k)[1], digits = 4),
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

linear = function(k) {
  z <- list(xx = coef(k)[1],
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

ggplot()+geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black",lty="solid"),se=FALSE,lwd=1,
                     method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=logwage,color="black",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=logwage,color="limegreen",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=logwage,color="blue",lty="dashed"),se=FALSE,lwd=1)+
  scale_color_identity(guide="legend",name= "Colors",
                       breaks=c("black","limegreen","blue"),
                       labels= c("All","Female","Male"))+
  scale_linetype_identity(guide=guide_legend(override.aes = list(col = 'black')),name="Line type",labels=c("Log Wage","Hatted lwage"))+
  annotate("text", x = 7, y = 9, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==0,])), colour="blue", size = 5, parse=TRUE)+
  annotate("text", x = 7, y = 8.8, label = linear(lm(yhat_all~ edu_yrs, data=ex_94AFM)), colour="black", size = 5, parse=TRUE)+
  annotate("text", x = 7, y = 8.6, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==1,])), colour="limegreen", size = 5, parse=TRUE)+
  annotate("text", x = 12, y = 9, 
           label = paste0("[",format(lm_mincer_m_1$`2000`$coefficients[2],digits=4),"]"), colour="blue", size = 5,
           fontface="italic")+
  annotate("text", x = 12, y = 8.8, 
           label = paste0("[",format(lm_mincer_all_1$`2000`$coefficients[2],digits=4),"]"), colour="black", size = 5,
           fontface="italic")+
  annotate("text", x = 12, y = 8.6, 
           label = paste0("[",format(lm_mincer_f_1$`2000`$coefficients[2],digits=4),"]"), colour="limegreen", size = 5,
           fontface="italic") +
    theme(panel.background = element_rect(fill = "thistle1",
                                        colour = "thistle1",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 1, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.75, linetype = 'solid',
                                        colour = "white"))+
  #geom_point(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen"),alpha=0.7)+ 
  #geom_point(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue"))+ 
  #geom_point(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black"))+ 
  ggtitle("Examining Predicted Mincerian Log Wage against Edu_Years for 2000")
ggsave("diag00.png", width = 10, height = 7,units = "in")
#############################


############################################
# 2008
df_m08 <- df_mincer %>% filter(YEAR==2008)
haven::write_dta(data=df_m08, path="df_mincer94.dta")

lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m08) 
smry_all_1$`2008`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m08[df_m08$female==1,]) 
smry_f_1$`2008`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m08[df_m08$female==0,]) 
smry_m_1$`2008`$coefficients[2,1]


ex_94all <- data.frame(IDIND=df_m08$IDIND,female=df_m08$female,logwage=log(df_m08$wage),edu_yrs=df_m08$edu_yrs,yhat_all=lm_mincer_all_1[[13]]$fitted.values)
ex_94F   <- data.frame(IDIND=df_m08[df_m08$female==1,c("IDIND")],yhat_G=lm_mincer_f_1[[13]]$fitted.values)
ex_94M   <- data.frame(IDIND=df_m08[df_m08$female==0,c("IDIND")],yhat_G=lm_mincer_m_1[[13]]$fitted.values)
ex_94FM  <- rbind(ex_94F,ex_94M) %>% arrange(IDIND)
ex_94AFM <- inner_join(ex_94all,ex_94FM,by="IDIND")


linear = function(k) {
  z <- list(xx = format(coef(k)[1], digits = 4),
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

linear = function(k) {
  z <- list(xx = coef(k)[1],
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

ggplot()+geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black",lty="solid"),se=FALSE,lwd=1,
                     method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=logwage,color="black",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=logwage,color="limegreen",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=logwage,color="blue",lty="dashed"),se=FALSE,lwd=1)+
  scale_color_identity(guide="legend",name= "Colors",
                       breaks=c("black","limegreen","blue"),
                       labels= c("All","Female","Male"))+
  scale_linetype_identity(guide=guide_legend(override.aes = list(col = 'black')),name="Line type",labels=c("Log Wage","Hatted lwage"))+
  annotate("text", x = 9, y = 10.5, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==0,])), colour="blue", size = 5, parse=TRUE)+
  annotate("text", x = 9, y = 10.3, label = linear(lm(yhat_all~ edu_yrs, data=ex_94AFM)), colour="black", size = 5, parse=TRUE)+
  annotate("text", x = 9, y = 10.1, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==1,])), colour="limegreen", size = 5, parse=TRUE)+
  annotate("text", x = 14, y = 10.5, 
           label = paste0("[",format(lm_mincer_m_1$`2008`$coefficients[2],digits=4),"]"), colour="blue", size = 5,
           fontface="italic")+
  annotate("text", x = 14, y = 10.3, 
           label = paste0("[",format(lm_mincer_all_1$`2008`$coefficients[2],digits=4),"]"), colour="black", size = 5,
           fontface="italic")+
  annotate("text", x = 14, y = 10.1, 
           label = paste0("[",format(lm_mincer_f_1$`2008`$coefficients[2],digits=4),"]"), colour="limegreen", size = 5,
           fontface="italic") +
  theme(panel.background = element_rect(fill = "seashell",
                                    colour = "seashell",
                                    size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 1, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.75, linetype = 'solid',
                                        colour = "white"))+
  #geom_point(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen"),alpha=0.7)+ 
  #geom_point(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue"))+ 
  #geom_point(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black"))+ 
  ggtitle("Examining Predicted Mincerian Log Wage against Edu_Years for 2008")
ggsave("diag08.png", width = 10, height = 7,units = "in")
#############################
# 2018
df_m18 <- df_mincer %>% filter(YEAR==2018)
haven::write_dta(data=df_m18, path="df_mincer94.dta")

lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m18) 
smry_all_1$`2018`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m18[df_m18$female==1,]) 
smry_f_1$`2018`$coefficients[2,1]
lm(log(wage) ~ edu_yrs + exper + I(exper^2),data=df_m18[df_m18$female==0,]) 
smry_m_1$`2018`$coefficients[2,1]


ex_94all <- data.frame(IDIND=df_m18$IDIND,female=df_m18$female,logwage=log(df_m18$wage),edu_yrs=df_m18$edu_yrs,yhat_all=lm_mincer_all_1[[23]]$fitted.values)
ex_94F   <- data.frame(IDIND=df_m18[df_m18$female==1,c("IDIND")],yhat_G=lm_mincer_f_1[[23]]$fitted.values)
ex_94M   <- data.frame(IDIND=df_m18[df_m18$female==0,c("IDIND")],yhat_G=lm_mincer_m_1[[23]]$fitted.values)
ex_94FM  <- rbind(ex_94F,ex_94M) %>% arrange(IDIND)
ex_94AFM <- inner_join(ex_94all,ex_94FM,by="IDIND")


linear = function(k) {
  z <- list(xx = format(coef(k)[1], digits = 4),
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

linear = function(k) {
  z <- list(xx = coef(k)[1],
            yy = format(abs(coef(k)[2]), digits = 4),
            r2 = format(summary(k)$r.squared, digits = 3));
  if (coef(k)[2] >= 0)  {
    eq <- substitute(italic(hat(y)) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
  } else {
    eq <- substitute(italic(hat(y)) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)  
  }
  as.character(as.expression(eq));              
}

ggplot()+geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black",lty="solid"),se=FALSE,lwd=1,
                     method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue",lty="solid"),se=FALSE,lwd=1,
              method=lm,formula= y ~x)+
  geom_smooth(data=ex_94AFM,aes(x=edu_yrs,y=logwage,color="black",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=logwage,color="limegreen",lty="dashed"),se=FALSE,lwd=1)+
  geom_smooth(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=logwage,color="blue",lty="dashed"),se=FALSE,lwd=1)+
  scale_color_identity(guide="legend",name= "Colors",
                       breaks=c("black","limegreen","blue"),
                       labels= c("All","Female","Male"))+
  scale_linetype_identity(guide=guide_legend(override.aes = list(col = 'black')),name="Line type",labels=c("Log Wage","Hatted lwage"))+
  annotate("text", x = 7, y = 10.9, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==0,])), colour="blue", size = 5, parse=TRUE)+
  annotate("text", x = 7, y = 10.7, label = linear(lm(yhat_all~ edu_yrs, data=ex_94AFM)), colour="black", size = 5, parse=TRUE)+
  annotate("text", x = 7, y = 10.5, label = linear(lm(yhat_G~ edu_yrs, data=ex_94AFM[ex_94AFM$female==1,])), colour="limegreen", size = 5, parse=TRUE)+
  annotate("text", x = 12, y = 10.9, 
           label = paste0("[",format(lm_mincer_m_1$`2018`$coefficients[2],digits=4),"]"), colour="blue", size = 5,
           fontface="italic")+
  annotate("text", x = 12, y = 10.7, 
           label = paste0("[",format(lm_mincer_all_1$`2018`$coefficients[2],digits=4),"]"), colour="black", size = 5,
           fontface="italic")+
  annotate("text", x = 12, y = 10.5, 
           label = paste0("[",format(lm_mincer_f_1$`2018`$coefficients[2],digits=4),"]"), colour="limegreen", size = 5,
           fontface="italic") +
  theme(panel.background = element_rect(fill = "lemonchiffon",
                                        colour = "lemonchiffon",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 1, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.75, linetype = 'solid',
                                        colour = "white"))+
  #geom_point(data=ex_94AFM[ex_94AFM$female==1,],aes(x=edu_yrs,y=yhat_G,color="limegreen"),alpha=0.7)+ 
  #geom_point(data=ex_94AFM[ex_94AFM$female==0,],aes(x=edu_yrs,y=yhat_G,color="blue"))+ 
  #geom_point(data=ex_94AFM,aes(x=edu_yrs,y=yhat_all,color="black"))+ 
  ggtitle("Examining Predicted Mincerian Log Wage against Edu_Years for 2018")
ggsave("diag18.png", width = 10, height = 7,units = "in")
#############################
