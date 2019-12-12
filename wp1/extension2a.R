# extension2a.R

# This script uses df_mincer2, a dataframe generated from extension1d.R 

# Written by Suhas D. Parandekar, Tuesday, December 10, 2019 
# Updated by Suhas D. Parandekar, Wednesday, December 11, 2019 


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
library(stargazer)

# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Specify the default working directory for this script
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1")

##

# import the robust standard error function
# download.file(url="https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R",destfile = "robust_summary.R")
source("robust_summary.R")


## Run Murillo Regression

wp_18 <- df_mincer2 %>% filter(YEAR==2018) 
lm_dep1 <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2), data=wp_18)
summary(lm_dep1)

wp_12 <- df_mincer2 %>% filter(YEAR==2012) 
lm_dep2 <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2), data=wp_12)
summary(lm_dep)


summary(lm_dep1,robust = T)
summary(lm_dep2,robust = T)

# save robust standard errors
robust_se1 <- as.vector(summary(lm_dep1,robust = T)$coefficients[,"Std. Error"])

# print stargazer output with robust standard errors
stargazer(lm_dep1, lm_dep2, type = "text",se = list(robust_se1))




seq_year <- c(2018, 2012, 2006, 2003, 1998, 1994)
lm_dep <- vector("list", length(seq_year))
for(i in seq(length(seq_year))){
  lm_dep[[i]] <- lm(log(wage) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2),
                    data = df_mincer2[df_mincer2$YEAR == seq_year[i],])
  
}
names(lm_dep) <- seq_year

library(RCurl)
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"

download.file(url="https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R",
              destfile = "robust_summary.R")

eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),envir=.GlobalEnv)
eval(parse("robust_summary.R"))




stargazer(lm_dep[1],
          lm_dep[2],
          lm_dep[3],
          lm_dep[4],
          lm_dep[5],
          lm_dep[6],
          type = "latex",
          dep.var.caption = "",
          dep.var.labels.include = F,
          df = F,
          header = F,
          se = list(as.vector(summary(lm_dep[[i]],robust = T)$coefficients[,"Std. Error"])),
          column.labels = c("2018",
                            "2012", 
                            "2006", 
                            "2003",
                            "1998",
                            "1994"),
          covariate.labels = c("Educ, years",
                               "Exper",
                               "Exper squared",
                               "Educ X Exper"))



smry <-  lapply(, function(x) {lapply(x, summary)})


