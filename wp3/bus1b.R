# bus1b.R

# Looking at initial data from Moscow Oblast


options(scipen=999) # to supress scientific notation
Sys.setlocale("LC_ALL", "russian")

library(tidyr)
library(rio)
library(dplyr)

# Load the dataset

moblast <- readRDS("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3/moblast.rds")

moblast$ins_code2 <- as.factor(moblast$institutionType_code)
levels(moblast$ins_code2)

inst_type <- moblast %>% select(ins_code2, institutionType_name) %>% 
  distinct() %>% arrange(ins_code2)

export(inst_type, file = "instype.xlsx", format = "xlsx")