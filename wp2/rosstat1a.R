# prelims1a.R

# Contents of Rosstat

Sys.setlocale("LC_CTYPE", "russian")

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
library(stargazer)
library(Hmisc)
##########################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT"
setwd(wd) 

######################################### Data ###########################################################

rst_18 <- read.spss(file="rosstat_18.sav",
                    use.value.labels = TRUE,
                    use.missings=TRUE,
                    to.data.frame = TRUE)

# Now generate first table

stargazer(rst_18,
          type="latex",out="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/documentation/rst_18a.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          omit.summary.stat=c("p25","p75"),
          summary=TRUE)

# Generate a second table
latex(Hmisc::describe(rst_18), file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/documentation/rst_18a.pretex",size="small")

blix <- Hmisc::describe(rst_18)
sink("blix.txt", append=TRUE)
blix

### 
# End of file

