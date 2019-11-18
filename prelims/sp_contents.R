#sp_contents.R

## Template file for contents of a dataframe


library(tidyverse)
library(haven)
library(Hmisc) # for its summary function

########### 2003
# Read the data
res19a <- read_dta(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/WBES/downloads/Russia-2019-full-data.dta")
# save file for later use with load when needed
save(res19a,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/WBES/downloads/res19a.RData") 



# Now generate first table
library(stargazer)
stargazer(res19a,
          type="latex",out="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/WBES/documentation/res19a.tex",
          style="default",
          align=TRUE,
          digit.separator="",
          omit.summary.stat=c("p25","p75"),
          summary=TRUE)

# Generate a second table
latex(Hmisc::describe(res19a), file=
        "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/WBES/documentation/res19b.tex",size="small")

