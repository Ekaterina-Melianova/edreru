# bus1a.R

# Looking at initial data from Moscow Oblast


options(scipen=999) # to supress scientific notation
Sys.setlocale("LC_ALL", "russian")

library(tidyr)
library(rio)
library(dplyr)

moblast <-  rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/bus.gov/bus.gov_moscow_oblast/busgov_MO.dta")
glimpse(moblast)

# part <- moblast %>% sample_n(100)


# Now generate first table
library(stargazer)
stargazer(moblast,
          type="latex",out="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/bus.gov/documents/moblast1a.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          omit.summary.stat=c("p25","p75"),
          summary=TRUE)

# Generate a second table
library(Hmisc)
latex(Hmisc::describe(moblast), file=
        "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/bus.gov/documents/moblast1b.pretex",size="small")



