# extension1c.R
options(scipen=999) # to supress scientific notation
# WP1 extension along lines of Neuman-Weiss 1995.

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

# Some functions
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Specify the default working directory for this script
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2")

## Loading a table with routineness classification
mt19 <- rio::import("mt19_table.xlsx")

## Loading a df with gender-based categorization created in extension1b.R
df_dep_18_ <- readRDS("df_dep_18_.rds")
df_dep_18_$occup <- as.numeric(df_dep_18_$occup) # remove haven_labelled_spss class

df_dep_18 <- left_join(df_dep_18_, mt19,
                       by = c("occup" = "isco_08"))

# Creating routineness variables
df_dep_18$NRAIM = df_dep_18$NRA + df_dep_18$NRI + df_dep_18$NRM
df_dep_18$RCM = df_dep_18$RC + df_dep_18$RM

# On basis of three aggregates, I define three groups for analysis of depreciation
# Note: drti and drcm are identical
df_dep_18$drti <- discretize(df_dep_18$RTI,breaks=3,method="cluster",labels=c("Low","Medium","High"))
df_dep_18$dnraim <- discretize(df_dep_18$NRAIM,breaks=3,method="cluster",labels=c("Low","Medium","High"))
df_dep_18$drcm <- discretize(df_dep_18$RCM,breaks=3,method="cluster",labels=c("Low","Medium","High"))

# Note: drti and drcm are identical
table(df_dep_18$drti, df_dep_18$drcm)
table(df_dep_18$drti)
table(df_dep_18$dnraim)
table(df_dep_18$drcm)

saveRDS(df_dep_18, "df_dep_18.rds")

### 
# End of file

