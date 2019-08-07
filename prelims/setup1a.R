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

i05_94_1a <- read_sav("C:/Country/Russia/Data/RLMS/rawdata/Round_05_1994/r05i_os26c.sav")
# Save the file in RData format 
save(i05_94_1a, file = "C:/Country/Russia/Data/RLMS/edreru/dbank/i05_94_1a.rda")
