# ess1a.R

# Import 8 rounds of ESS data (available as of October 9, 2019)
# See https://www.europeansocialsurvey.org/about/participating_countries.html for list of countries

library(dplyr)
library(essurvey)

# First you have to register your e-mail through ESS website
set_email("sparandekar@yahoo.com.ar")

# Download from ESS server
all_rounds <- import_all_rounds()

# Extract Round 1
round1 <- as.data.frame(all_rounds[1])
glimpse(round1)

round8 <- as.data.frame(all_rounds[8])
glimpse(round8)

# Get country characters

table(round8$cntry)
# AT   BE   CH   CZ   DE   EE   ES   FI   FR   GB   HU   IE   IL   IS   IT   LT   NL   NO   PL   PT   RU   SE   SI 

# To be continued....

# save(all_rounds,file="C:/Country/Russia/Data/SEABYTE/ESS/downloads/ess_all_rounds.rdata ")

# To use the data; 
load("C:/Country/Russia/Data/SEABYTE/ESS/downloads/ess_all_rounds.rdata")

