# ess1b.R
# Using the essa allrounds data downloaded from essa Wednesday, October 09, 2019 
# Descriptive summaries of the data

library(dplyr) # for data mungeing
library(stargazer) # for tables
library(Hmisc) # deprecated - perhaps not in CRAN, for summary function of descriptives


# To use the data; 
load("C:/Country/Russia/Data/SEABYTE/ESS/downloads/ess_all_rounds.rdata")

# Start with round 8

round8 <- as.data.frame(all_rounds[8])
glimpse(round8)

# Description of data

# I generate means table */
stargazer(round8,
          type="latex",out="C:/Country/Russia/Data/SEABYTE/latex/round8a.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          omit.summary.stat = c("p25","p75"),
          summary=TRUE)

# I generate summary table from deprecared Hmisc function
latex(describe(round8), file="C:/Country/Russia/Data/SEABYTE/latex/round8a_2.pretex",size="small")
