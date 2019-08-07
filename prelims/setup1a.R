# setup1a.R
# Written by Suhas; Wednesday, August 7, 2019

# Setting up RLMS data from 1994 (Round 5) to 2017 (Round 26)

library(tidyverse)

# tidyverse includes haven package for importing SPSS files, but it has to be loaded separately
library(haven)

# Reading from google drive synced rawdata file 
h05_94_1a <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_05_1994/r05h_os26c.sav")
# Save the file in RData format 
save(h05_94_1a, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/h05_94_1a.rda")

# This is such an old-fashioned way to load files!

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# A task for EM will be to write a small and efficient routine that can loop through data for all the rounds at one go
# But only if it is relatively easy to do so
# Eventually we want to develop an RLMS package where loading will be easy
# There would be a function that points to the url where data is available
# and select - either by round number or year, also variables etc. 

i05_94_1a <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_05_1994/r05i_os26c.sav")
# Save the file in RData format 
save(i05_94_1a, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/i05_94_1a.rda")

###
h06_95_1b <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_06_1995/r06h_os26c.sav")
# Save the file in RData format 
save(h06_95_1b, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/h06_95_1b.rda")
i06_95_1b <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_06_1995/r06i_os26b.sav")
# Save the file in RData format 
save(i06_95_1b, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/i06_95_1b.rda")


###
h07_96_1c <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_07_1996/r07h_os26c.sav")
# Save the file in RData format 
save(h07_96_1c, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/h07_96_1c.rda")
i07_96_1c <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_07_1996/r07i_os26b.sav")
# Save the file in RData format 
save(i07_96_1c, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/i07_96_1c.rda")


glimpse(i07_96_1a)

min(i05_94_1a$idind)

