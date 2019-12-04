# extension1d.R
options(scipen=999) # to supress scientific notation
# Arrazola and De Havia / Weber et al 

library(plyr); library(dplyr)
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
library(psych)
library(stringi)
library(sjPlot)
library(sjmisc)
library(segregation)
library(reshape2)
library(arules) 

# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Specify the default working directory for this script
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1")

##

# Adjusting to prices in 2018
mt19 <- rio::import("mt19_table.xlsx")

df_mincer2 <- left_join(df_mincer,mt19,by = c("occup" = "isco_08"))

df_mincer2$NRAIM=df_mincer2$NRA+df_mincer2$NRI+df_mincer2$NRM
df_mincer2$RCM=df_mincer2$RC+df_mincer2$RM

df_18 <- df_mincer2 %>% filter(YEAR==2018) 

df_18f <- df_18 %>% select(IDIND, wage, edu_4,exper,female,edu_yrs)  %>% filter(female==1) %>%
               mutate(lnwage=log(wage), d_voc=ifelse(edu_4=="Vocational",1,0),
                      d_uni=ifelse(edu_4=="Higher",1,0), tlabor0=64-edu_yrs)  # using 64 for now, will need to adjust
df_18f <- haven::zap_labels(df_18f) # Else rio gives error message


df_18m <- df_18 %>% select(IDIND, wage, edu_4,exper,female,edu_yrs)  %>% filter(female==0) %>%
  mutate(lnwage=log(wage),d_voc=ifelse(edu_4=="Vocational",1,0),
         d_uni=ifelse(edu_4=="Higher",1,0), tlabor0=64-edu_yrs)  # using 64 for now, will need to adjusttlabor0=64-edu_yrs) # using 64 for now, will need to adjust
df_18m <- haven::zap_labels(df_18m) # Else rio gives error message

rio::export(df_18f, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_18f.dta",format="stata")
rio::export(df_18m, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_18m.dta",format="stata")



# Try for model with covariates - just placeholder
df_18fz <- df_18 %>% select(IDIND, wage, edu_4,exper,female,edu_yrs,RTI)  %>% filter(female==1) %>%
  mutate(lnwage=log(wage), d_voc=ifelse(edu_4=="Vocational",1,0),
         d_uni=ifelse(edu_4=="Higher",1,0), tlabor0=64-edu_yrs)  # using 64 for now, will need to adjust
df_18fz <- haven::zap_labels(df_18fz) # Else rio gives error message

rio::export(df_18fz, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata/df_18fz.dta",format="stata")









%>% mutate(summer=1) %>% 
  group_by(occup) %>% 
  summarize(n_occ=sum(summer)) %>% arrange(desc(n_occ)) %>% 
  mutate(cn_occ=cumsum(n_occ))


junk <- df_temp1 %>% filter(YEAR==2018|YEAR==2006) %>% 
  select(IDIND, ID_H,ID_I,AGE,YEAR) %>% arrange(IDIND,YEAR) %>% 
  distinct(IDIND,.keep_all = T)

junk <- df_temp1 %>% filter(YEAR==1998|YEAR==2006) %>% 
  select(IDIND, ID_H,ID_I,AGE,YEAR) %>% arrange(IDIND,YEAR) %>% 
  distinct(IDIND,.keep_all = T)


df_18$drti <- discretize(df_18$RTI,breaks=3,method="cluster",labels=c("Low","Medium","High"))
df_18$dnraim <- discretize(df_18$NRAIM,breaks=3,method="cluster",labels=c("Low","Medium","High"))
df_18$drcm <- discretize(df_18$RCM,breaks=3,method="cluster",labels=c("Low","Medium","High"))

table(df_18$drti)
table(df_18$dnraim)
table(df_18$drcm)


