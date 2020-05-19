# mincer2a.R
# A file to generate the rest of the variables for the mincer equation based on RLMS and run the equation.

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

############################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite"
setwd(wd) 

# Some functions -later to be edreru package
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

# saveRDS(zdf_,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/zdf_.rds")
zdf_ <- readRDS("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/zdf_.rds")

dbDisconnect(db)
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

df_mincer <- df[, c("REGION", "IDIND", "YEAR", "edu_4", "wage", 'EDUC',
                    "exper", "non_russ", "female",
                    "edu_yrs", 'AGE', 'J72_5C', 'J72_6A', 'J72_4C', 'J72_3C',
                    'J70', 'J70_1', 'J72_2C', 'J72_18A')]
summary(df_mincer)
glimpse(df_m)

# Filtering the missings left
df_mincer <- df_mincer %>%
  filter(!is.na(wage) & !is.na(exper) & wage > 0)

# Filtering 3 education levels
df_mincer <- df_mincer[df_mincer$edu_4>0,]
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

latex(desc_rst)

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
                           labels = c("Males",
                                      "Females",
                                      "Total"))
# Factor corrected by SP - earlier version of paper 
# Prior to May 14 showed total in middle which makes sense

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
  scale_color_manual(values = c("black","purple","darkgray")) +
  #scale_shape_manual(values=c(2,4)) +
  scale_x_discrete(breaks = x_axis) +
  ylab("Rate of returns, %") +
  xlab("Year")

wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)
# Saving
ggsave("re_edu.png", width = 10, height = 7,
       units = "in")



RoREs_edu2 <- RoREs_edu %>% filter(variable=="Males"|variable=="Females")

##################
# Plotting all
ggplot(RoREs_edu2, aes(YEAR, value, group = variable, color = variable,
                      shape = variable)) +
   geom_smooth(se = F, method = 'loess') +
  scale_y_continuous(limits = c(5, 15)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.70,0.70),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.key = element_rect(size = 20),
        legend.key.width = unit(2, "cm")) + 
    scale_color_manual(values = c("black","purple")) +
  #scale_shape_manual(values=c(2,4)) +
  scale_x_discrete(breaks = x_axis) +
  ylab("Rate of returns, %") +
  xlab("Year")

wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1"
setwd(wd)
# Saving
ggsave("re_edu2.png", width = 10, height = 7,
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





















