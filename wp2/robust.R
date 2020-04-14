# robust.R
# https://economictheoryblog.com/2016/08/08/robust-standard-errors-in-r/ 

# start with an empty workspace
rm(list=ls())

# load necessary packages for importing the function
library(RCurl)
# load necessary packages for the example
library(gdata) 
library(zoo)

# import the robust standard error function
download.file(url="https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R",destfile = "robust_summary.R")
source("robust_summary.R")


# download data set for example
#download.file(url="https://economictheoryblog.files.wordpress.com/2016/08/data.xlsx",destfile="url_data.xls",mode="wb")
#data <- read.xls(gsub("s:",":",url_data))

data <-  rio::import("https://economictheoryblog.files.wordpress.com/2016/08/data.xlsx")

# estimate simple linear model
reg <- lm(weight ~ lag_calories+lag_cycling+
            I(lag_calories*lag_cycling), 
          data=data)

greg <- lm(weight ~ lag_calories+I(lag_cycling^2)+
            I(lag_calories*lag_cycling), 
          data=data)

# use new summary function
summary(reg,robust = T)
summary(reg,robust = F)


# create stargazer output with robust standard errors
require("stargazer")

# save robust standard errors
robust_se <- as.vector(summary(reg,robust = T)$coefficients[,"Std. Error"])

# print stargazer output with robust standard errors
stargazer(reg, greg, type = "text",se = list(robust_se))



# the last command prints the stargazer output (in this case as tex t)
# with robust standard errors. 
