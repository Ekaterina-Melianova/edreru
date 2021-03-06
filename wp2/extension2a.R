# extension2a.R

# This script uses df_mincer2, a dataframe generated from extension1d.R 

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

# sparkTable package was removed froom CRAN, install from the archive
# library(devtools)
# install_url('https://cran.r-project.org/src/contrib/Archive/sparkTable/sparkTable_1.3.0.tar.gz')
library(sparkTable)

# Some functions
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Specify the default working directory for this script
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2")

# import the robust standard error function
# download.file(url="https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R",destfile = "robust_summary.R")
source("robust_summary.R")

# Data
df_dep_18 <- readRDS("df_dep_18.rds")
df_mincer2 <- readRDS("df_mincer2.rds")

################################### Run Murillo Regression ##################################

########### A. General model 

# The sequence of years of interest: the ends (1994 and 2018); the sort of diffused peak 
# (2003 and 2006), and then half way points to the ends (2012) and (2003)
seq_year <- c(1994, 1998, 2003, 2006, 2012, 2018)

# Empty list for the regression output
lm_dep_all <- vector("list", length(seq_year))
lm_dep_f =  lm_dep_m =  lm_dep_all

# Formula
formula = as.formula("log(wage) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2)")
# Running regressions
for(i in seq(length(seq_year))){
  lm_dep_all[[i]] <- lm(formula, data = df_mincer2[df_mincer2$YEAR == seq_year[i],])
  lm_dep_f[[i]] <- lm(formula, data = df_mincer2[df_mincer2$YEAR == seq_year[i] &
                                                   df_mincer2$female == 1,])
  lm_dep_m[[i]] <- lm(formula, data = df_mincer2[df_mincer2$YEAR == seq_year[i] &
                                                   df_mincer2$female == 0,])
}
  
########### B.I By industry

lm_by_cat <- function(df, cat, 
                      female = "female",
                      formula = as.formula("log(wage) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2)"))
  {
    # Empty lists for regression outputs
    n_cat <- length(table(df[, cat]))
    nam <- rep(c("all", "f", "m"), n_cat)
        for(i in 1:length(nam)){
      for (j in 1:n_cat){
        assign(paste("lm_dep", j, nam[i], sep = "_"),
               list())   
      }
    }
    lm_dep_all <- list()
    lm_dep_f <- list()
    lm_dep_m <- list()
    
    # Running regressions separately for each category 
    for (j in 1:n_cat){
      lm_dep_all[[j]] <- assign(paste("lm_dep", j, "all", sep = "_"),
                 lm(formula,
                    data = df_dep_18[df_dep_18[, cat] == names(table(df_dep_18[, cat]))[j],]))
      lm_dep_f[[j]] <- assign(paste("lm_dep", j, "f", sep = "_"),
                 lm(formula,
                    data = df_dep_18[df_dep_18[, cat] == names(table(df_dep_18[, cat]))[j] &
                                       df_dep_18[, female] == 1,]))
      lm_dep_m[[j]] <- assign(paste("lm_dep", j, "m", sep = "_"),
                 lm(formula,
                    data = df_dep_18[df_dep_18[, cat] == names(table(df_dep_18[, cat]))[j] &
                                       df_dep_18[, female] == 0,]))
    }

    return(list(lm_dep_all, lm_dep_f, lm_dep_m))
}

lm_dep_ind <- lm_by_cat(df = df_dep_18, cat = "indcat")

########### B.II By 2-digit occupations
lm_dep_ocp <- lm_by_cat(df = df_dep_18, cat = "ocpcat18")

########### C. By routineness classification
df_dep_18$drti <- as.character(df_dep_18$drti)
df_dep_18$dnraim <- as.character(df_dep_18$dnraim)
df_dep_18$drcm <- as.character(df_dep_18$drcm)

lm_dep_drti <- lm_by_cat(df = df_dep_18, cat = "drti")
lm_dep_dnraim <- lm_by_cat(df = df_dep_18, cat = "dnraim")

############################################# Latex output #########################################
########### A. General model 
stargazer(lm_dep_all[1],
          lm_dep_all[2],
          lm_dep_all[3],
          lm_dep_all[4],
          lm_dep_all[5],
          lm_dep_all[6],
          type = "latex",
          dep.var.caption = "",
          dep.var.labels.include = F,
          df = F,
          header = F,
          intercept.bottom = F,
          se = list(as.vector(summary(lm_dep_all[[1]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_all[[2]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_all[[3]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_all[[4]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_all[[5]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_all[[6]],robust = T)$coefficients[,"Std. Error"])),
          column.labels = c("1994", "1998", "2003", "2006", "2012", "2018"),
          covariate.labels = c("Constant",
                               "Educ, years",
                               "Exper",
                               "Exper squared",
                               "Educ X Exper"))


#########################################################################
########### A. General model 
# Averages and 3 rows of Vignoli
library(tables)

# Coefficients for exper
exp_2_coef <- c(as.vector(lm_dep_all[[1]]$coefficients['I(exper^2)']),
           as.vector(lm_dep_all[[2]]$coefficients['I(exper^2)']),
           as.vector(lm_dep_all[[3]]$coefficients['I(exper^2)']),
           as.vector(lm_dep_all[[4]]$coefficients['I(exper^2)']),
           as.vector(lm_dep_all[[5]]$coefficients['I(exper^2)']),
           as.vector(lm_dep_all[[6]]$coefficients['I(exper^2)']))

# Coefficients for edu_yrs
edu_yrs_coef <- c(as.vector(lm_dep_all[[1]]$coefficients['edu_yrs:exper']),
                as.vector(lm_dep_all[[2]]$coefficients['edu_yrs:exper']),
                as.vector(lm_dep_all[[3]]$coefficients['edu_yrs:exper']),
                as.vector(lm_dep_all[[4]]$coefficients['edu_yrs:exper']),
                as.vector(lm_dep_all[[5]]$coefficients['edu_yrs:exper']),
                as.vector(lm_dep_all[[6]]$coefficients['edu_yrs:exper']))

# Examine the significance
confint(lm_dep_all[[1]], level = 0.99)
confint(lm_dep_all[[2]])
confint(lm_dep_all[[3]])
confint(lm_dep_all[[4]], level = 0.99)
confint(lm_dep_all[[5]])
confint(lm_dep_all[[6]], level = 0.9)

# creating df for further averaging edu_yrs and exper by years
edu_exper <- df_mincer2[df_mincer2$YEAR %in% 
                          seq_year, c("YEAR", "edu_yrs", "exper")]

# Combining averages and coefficients to a data.frame
edu_exp_mean <- rbind(
  dcast(melt(edu_exper, id.vars = "YEAR"),
        variable ~ YEAR,
        fun.aggregate = mean)[,-1],
  exp_2_coef,
  edu_yrs_coef)

# Computing depreciation rates by formulas: 2*pi_2*T and pi_1*S
# exper
edu_exp_mean[3,] <- abs(edu_exp_mean[3,]*2*edu_exp_mean[1,]*100)

# edu_yrs
edu_exp_mean[4,] <- abs(edu_exp_mean[4,]*edu_exp_mean[2,]*100)

# From 2003 to 2018 DR is non-significant => 0
edu_exp_mean[4, 4] <- 0
edu_exp_mean[4, 5] <- 0
edu_exp_mean[4, 6] <- 0

# Human Capital
edu_exp_mean[5,] <- edu_exp_mean[3,] + edu_exp_mean[4,]

# Rounding
edu_exp_mean <- round(edu_exp_mean, 2)

# Naming
Statistic <- c("Education, mean",
              "Experience, mean",
              "DR Experience, %",
              "DR Education, %",
              "DR Human Capital, %")
# Adding rownames
edu_exp_mean <- cbind.data.frame(Statistic, edu_exp_mean)
# Latex 
xtable::xtable(edu_exp_mean)

#### A. General Model. Output for Sparklines
############################################
# First I need to get the edu_exp_mean data into long form for sparklines
edu_all_ <- edu_exp_mean[c(3,4,5),] %>% rename("variable"="Statistic")
(edu_all <- melt(edu_all_,id.vars="variable",variable.name = "time"))

## sparkTable needs two parameters in addition to data - content and VarType
content <- list(
  function(x) {round(mean(x), 2)}, newSparkLine())
names(content) <- paste('column',1:2,sep='')
varType <- rep('value',2)

# sparkTable needs to run function reshapeExt on long form of data
edu_all <- edu_all[,c('variable','value','time')]
(xx <- reshapeExt(edu_all, varying=list(2))) # I do not understand the varying parameter

# reshapeExt at up the time and added an id, I just fix that
xx$time <- edu_all$time
xx$id <- NULL

# Generate the sparkTable
x1 <- newSparkTable(xx, content, varType)

# I save the sparklines, there is a latex table generated spkline.tex
# from which I only use the preamble and graphics command in the latex table
# main output are three sparklines 
export(x1, outputType="tex", 
       filename="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/sparklines/spkline",
       graphNames="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/sparklines/all")
#######################################
########### A1F. General model - Female
stargazer(lm_dep_f[1],
          lm_dep_f[2],
          lm_dep_f[3],
          lm_dep_f[4],
          lm_dep_f[5],
          lm_dep_f[6],
          type = "latex",
          dep.var.caption = "",
          dep.var.labels.include = F,
          df = F,
          header = F,
          intercept.bottom = F,
          se = list(as.vector(summary(lm_dep_f[[1]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_f[[2]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_f[[3]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_f[[4]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_f[[5]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_f[[6]],robust = T)$coefficients[,"Std. Error"])),
          column.labels = c("1994", "1998", "2003", "2006", "2012", "2018"),
          covariate.labels = c("Constant",
                               "Educ, years",
                               "Exper",
                               "Exper squared",
                               "Educ X Exper"))


#############
##############
########### A2F. General model  - By gender - female 
# Averages and 3 rows of Vignoli
library(tables)

# Coefficients for exper
exp_2_coef <- c(as.vector(lm_dep_f[[1]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_f[[2]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_f[[3]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_f[[4]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_f[[5]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_f[[6]]$coefficients['I(exper^2)']))

# Coefficients for edu_yrs
edu_yrs_coef <- c(as.vector(lm_dep_f[[1]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_f[[2]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_f[[3]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_f[[4]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_f[[5]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_f[[6]]$coefficients['edu_yrs:exper']))

# Examine the significance
confint(lm_dep_f[[1]], level = 0.99)
confint(lm_dep_f[[2]])
confint(lm_dep_f[[3]])
confint(lm_dep_f[[4]], level = 0.99)
confint(lm_dep_f[[5]])
confint(lm_dep_f[[6]], level = 0.9)

# creating df for further averaging edu_yrs and exper by years
df_mincer2f <- df_mincer2 %>% filter(female==1)
edu_exper_f <- df_mincer2f[df_mincer2f$YEAR %in% 
                          seq_year, c("YEAR", "edu_yrs", "exper")]

# Combining averages and coefficients to a data.frame
edu_exp_mean <- rbind(
  dcast(melt(edu_exper_f, id.vars = "YEAR"),
        variable ~ YEAR,
        fun.aggregate = mean)[,-1],
  exp_2_coef,
  edu_yrs_coef)

# Computing depreciation rates by formulas: 2*pi_2*T and pi_1*S
# exper
edu_exp_mean[3,] <- abs(edu_exp_mean[3,]*2*edu_exp_mean[1,]*100)

# edu_yrs
edu_exp_mean[4,] <- abs(edu_exp_mean[4,]*edu_exp_mean[2,]*100)

# From 2003 to 2018 DR is non-significant => 0
edu_exp_mean[4, 4] <- 0
edu_exp_mean[4, 5] <- 0
edu_exp_mean[4, 6] <- 0

# Human Capital
edu_exp_mean[5,] <- edu_exp_mean[3,] + edu_exp_mean[4,]

# Rounding
edu_exp_mean <- round(edu_exp_mean, 2)

# Naming
Statistic <- c("Education, mean",
               "Experience, mean",
               "DR Experience, %",
               "DR Education, %",
               "DR Human Capital, %")
# Adding rownames
edu_exp_mean <- cbind.data.frame(Statistic, edu_exp_mean)
# Latex 
xtable::xtable(edu_exp_mean)
###########################################
#### A2F. General Model. Output for Sparklines
############################################
# First I need to get the edu_exp_mean data into long form for sparklines
edu_female_ <- edu_exp_mean[c(3,4,5),] %>% rename("variable"="Statistic")
(edu_female <- melt(edu_female_,id.vars="variable",variable.name = "time"))

## sparkTable needs two parameters in addition to data - content and VarType
content <- list(
  function(x) { round(mean(x),2) },newSparkLine())
names(content) <- paste('column',1:2,sep='')
varType <- rep('value',2)

# sparkTable needs to run function reshapeExt on long form of data
edu_female <- edu_female[,c('variable','value','time')]
(xx <- reshapeExt(edu_female, varying=list(2))) # I do not understand the varying parameter

# reshapeExt at up the time and added an id, I just fix that
xx$time <- edu_female$time
xx$id <- NULL

# Generate the sparkTable
x1 <- newSparkTable(xx, content, varType)

# I save the sparklines, there is a latex table generated spkline.tex
# from which I only use the preamble and graphics command in the latex table
# main output are three sparklines 
export(x1, outputType="tex", 
       filename="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/sparklines/spkline",
       graphNames="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/sparklines/female")
#######################################


#######################################
########### A1M. General model - Male
stargazer(lm_dep_m[1],
          lm_dep_m[2],
          lm_dep_m[3],
          lm_dep_m[4],
          lm_dep_m[5],
          lm_dep_m[6],
          type = "latex",
          dep.var.caption = "",
          dep.var.labels.include = F,
          df = F,
          header = F,
          intercept.bottom = F,
          se = list(as.vector(summary(lm_dep_m[[1]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_m[[2]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_m[[3]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_m[[4]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_m[[5]],robust = T)$coefficients[,"Std. Error"]),
                    as.vector(summary(lm_dep_m[[6]],robust = T)$coefficients[,"Std. Error"])),
          column.labels = c("1994", "1998", "2003", "2006", "2012", "2018"),
          covariate.labels = c("Constant",
                               "Educ, years",
                               "Exper",
                               "Exper squared",
                               "Educ X Exper"))


#############
##############
########### A2M. General model  - By gender - male 
# Averages and 3 rows of Vignoli
library(tables)

# Coefficients for exper
exp_2_coef <- c(as.vector(lm_dep_m[[1]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_m[[2]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_m[[3]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_m[[4]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_m[[5]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_m[[6]]$coefficients['I(exper^2)']))

# Coefficients for edu_yrs
edu_yrs_coef <- c(as.vector(lm_dep_m[[1]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_m[[2]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_m[[3]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_m[[4]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_m[[5]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_m[[6]]$coefficients['edu_yrs:exper']))

# Examine the significance
confint(lm_dep_m[[1]], level = 0.99)
confint(lm_dep_m[[2]])
confint(lm_dep_m[[3]])
confint(lm_dep_m[[4]], level = 0.99)
confint(lm_dep_m[[5]])
confint(lm_dep_m[[6]], level = 0.9)

# creating df for further averaging edu_yrs and exper by years
df_mincer2m <- df_mincer2 %>% filter(female==0)
edu_exper_m <- df_mincer2m[df_mincer2m$YEAR %in% 
                             seq_year, c("YEAR", "edu_yrs", "exper")]

# Combining averages and coefficients to a data.frame
edu_exp_mean <- rbind(
  dcast(melt(edu_exper_m, id.vars = "YEAR"),
        variable ~ YEAR,
        fun.aggregate = mean)[,-1],
  exp_2_coef,
  edu_yrs_coef)

# Computing depreciation rates by formulas: 2*pi_2*T and pi_1*S
# exper
edu_exp_mean[3,] <- abs(edu_exp_mean[3,]*2*edu_exp_mean[1,]*100)

# edu_yrs
edu_exp_mean[4,] <- abs(edu_exp_mean[4,]*edu_exp_mean[2,]*100)

# From 2003 to 2018 DR is non-significant => 0
edu_exp_mean[4, 4] <- 0
edu_exp_mean[4, 5] <- 0
edu_exp_mean[4, 6] <- 0

# Human Capital
edu_exp_mean[5,] <- edu_exp_mean[3,] + edu_exp_mean[4,]

# Rounding
edu_exp_mean <- round(edu_exp_mean, 2)

# Naming
Statistic <- c("Education, mean",
               "Experience, mean",
               "DR Experience, %",
               "DR Education, %",
               "DR Human Capital, %")
# Adding rownames
edu_exp_mean <- cbind.data.frame(Statistic, edu_exp_mean)
# Latex 
xtable::xtable(edu_exp_mean)

###########################################
#### A2M. General Model. Output for Sparklines
############################################
# First I need to get the edu_exp_mean data into long form for sparklines
edu_male_ <- edu_exp_mean[c(3,4,5),] %>% rename("variable"="Statistic")
(edu_male <- melt(edu_male_,id.vars="variable",variable.name = "time"))

## sparkTable needs two parameters in addition to data - content and VarType
content <- list(
  function(x) { round(mean(x),2) },newSparkLine())
names(content) <- paste('column',1:2,sep='')
varType <- rep('value',2)

# sparkTable needs to run function reshapeExt on long form of data
edu_male <- edu_male[,c('variable','value','time')]
(xx <- reshapeExt(edu_male, varying=list(2))) # I do not understand the varying parameter

# reshapeExt at up the time and added an id, I just fix that
xx$time <- edu_male$time
xx$id <- NULL

# Generate the sparkTable
x1 <- newSparkTable(xx, content, varType)

# I save the sparklines, there is a latex table generated spkline.tex
# from which I only use the preamble and graphics command in the latex table
# main output are three sparklines 
export(x1, outputType="tex", 
       filename="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/sparklines/spkline",
       graphNames="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp2/sparklines/male")
#######################################


###########
########### B.I By industry and B.II By 2-digit occupations
###########

# Coefficients for exper
exp_2_coef <- c(as.vector(lm_dep_ind[[1]][[1]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_ind[[1]][[2]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_ocp[[1]][[1]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_ocp[[1]][[2]]$coefficients['I(exper^2)']))

# Coefficients for edu_yrs
edu_yrs_coef <- c(as.vector(lm_dep_ind[[1]][[1]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_ind[[1]][[2]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_ocp[[1]][[1]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_ocp[[1]][[2]]$coefficients['edu_yrs:exper']))

# Examine the significance: intervals intersect, no difference between groups
confint(lm_dep_ind[[1]][[1]], level = 0.90)
confint(lm_dep_ind[[1]][[2]], level = 0.90)
confint(lm_dep_ocp[[1]][[1]])
confint(lm_dep_ocp[[1]][[2]])

# Creating df for further averaging edu_yrs and exper by years
edu_exper <- df_dep_18[, c("indcat", "ocpcat18", "edu_yrs", "exper")]

# Average for edu_yrs
edu_yrs_means <- cbind(dcast(melt(edu_exper, id.vars = "indcat", measure.vars = "edu_yrs"),
                             variable ~ indcat,
                             fun.aggregate = mean)[-c(1, 4:5)],
                       dcast(melt(edu_exper, id.vars = "ocpcat18", measure.vars = "edu_yrs"),
                             variable ~ ocpcat18,
                             fun.aggregate = mean)[-c(1, 4:5)])

# Average for exper
exper_means <- cbind(dcast(melt(edu_exper, id.vars = "indcat", measure.vars = "exper"),
                           variable ~ indcat,
                           fun.aggregate = mean)[-c(1, 4:5)],
                     dcast(melt(edu_exper, id.vars = "ocpcat18", measure.vars = "exper"),
                           variable ~ ocpcat18,
                           fun.aggregate = mean)[-c(1, 4:5)])

# Combining averages and coefficients to a data.frame
edu_exp_mean <- rbind(edu_yrs_means, exper_means,
                            exp_2_coef, edu_yrs_coef)

# Computing depreciation rates by formulas: 2*pi_2*T and pi_1*S
# exper
edu_exp_mean[3,] <- abs(edu_exp_mean[3,]*2*edu_exp_mean[1,]*100)

# edu_yrs: nothing is non-significant => 0
edu_exp_mean[4, 1] <- 0
edu_exp_mean[4, 2] <- 0
edu_exp_mean[4, 3] <- 0
edu_exp_mean[4, 4] <- 0

# Human Capital
edu_exp_mean[5,] <- edu_exp_mean[3,] + edu_exp_mean[4,]

# Rounding
edu_exp_mean <- round(edu_exp_mean, 2)

# Naming
Statistic <- c("Education, mean",
               "Experience, mean",
               "DR Experience, %",
               "DR Education, %",
               "DR Human Capital, %")
# Adding rownames
edu_exp_mean <- cbind.data.frame(Statistic, edu_exp_mean)
# Latex 
xtable::xtable(edu_exp_mean)

###########
########### C. By routineness classification
###########

# Coefficients for exper
exp_2_coef <- c(as.vector(lm_dep_drti[[1]][[1]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_drti[[1]][[2]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_drti[[1]][[3]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_dnraim[[1]][[1]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_dnraim[[1]][[2]]$coefficients['I(exper^2)']),
                as.vector(lm_dep_dnraim[[1]][[3]]$coefficients['I(exper^2)']))

# Coefficients for edu_yrs
edu_yrs_coef <- c(as.vector(lm_dep_drti[[1]][[1]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_drti[[1]][[2]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_drti[[1]][[3]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_dnraim[[1]][[1]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_dnraim[[1]][[2]]$coefficients['edu_yrs:exper']),
                  as.vector(lm_dep_dnraim[[1]][[3]]$coefficients['edu_yrs:exper']))

# Examine the significance: intervals intersect, no difference between groups
confint(lm_dep_drti[[1]][[1]])
confint(lm_dep_drti[[1]][[2]])
confint(lm_dep_drti[[1]][[3]])
confint(lm_dep_dnraim[[1]][[1]])
confint(lm_dep_dnraim[[1]][[2]])
confint(lm_dep_dnraim[[1]][[3]])

# creating df for further averaging edu_yrs and exper by years
edu_exper <- df_dep_18[, c("drti", "dnraim", "edu_yrs", "exper")]

# Average for edu_yrs
edu_yrs_means <- cbind(dcast(melt(edu_exper, id.vars = "drti", measure.vars = "edu_yrs"),
                             variable ~ drti,
                             fun.aggregate = mean)[-c(1, 5)],
                       dcast(melt(edu_exper, id.vars = "dnraim", measure.vars = "edu_yrs"),
                             variable ~ dnraim,
                             fun.aggregate = mean)[-c(1, 5)])

# Average for exper
exper_means <- cbind(dcast(melt(edu_exper, id.vars = "drti", measure.vars = "exper"),
                           variable ~ drti,
                           fun.aggregate = mean)[-c(1, 5)],
                     dcast(melt(edu_exper, id.vars = "dnraim", measure.vars = "exper"),
                           variable ~ dnraim,
                           fun.aggregate = mean)[-c(1, 5)])

# Combining averages and coefficients to a data.frame
edu_exp_mean <- rbind(edu_yrs_means, exper_means,
                            exp_2_coef, edu_yrs_coef)

# Computing depreciation rates by formulas: 2*pi_2*T and pi_1*S
# exper
edu_exp_mean[3,] <- abs(edu_exp_mean[3,]*2*edu_exp_mean[1,]*100)

# edu_yrs: nothing is non-significant => 0
for (i in 1:ncol(edu_exp_mean)){
  edu_exp_mean[4, i] <- 0
  
}

# Human Capital
edu_exp_mean[5,] <- edu_exp_mean[3,] + edu_exp_mean[4,]

# Rounding
edu_exp_mean <- round(edu_exp_mean, 2)

# Naming
Statistic <- c("Education, mean",
               "Experience, mean",
               "DR Experience, %",
               "DR Education, %",
               "DR Human Capital, %")
col <- c("Measure", "drti", "drti", "drti",
         "dnraim", "dnraim", "dnraim")

# Adding names
edu_exp_mean <- cbind.data.frame(Statistic, edu_exp_mean)
edu_exp_mean$Statistic <- as.character(edu_exp_mean$Statistic)
edu_exp_mean <- rbind.data.frame(col, edu_exp_mean)

# Latex 
xtable::xtable(edu_exp_mean)

### 
# End of file


