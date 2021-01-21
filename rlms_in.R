# rlms_in.R

# This script is used to create an SQLITE relational database from RLMS
# data downloaded from the website https://www.hse.ru/fileaccess/e131584301/data/2020/09/09/1116873148/USER_RLMS-HSE_IND_1994_2019_v2_eng_SPSS.zip 

# The script produces separate .csv files for each variable in the RLMS dataset.
# To create an SQLite relational database, the csv. files should be imported 
# to a database using the DB Browser for SQLite software.

library(sqldf)
library(XLConnectJars)

# Working directory - please specify your own 
setwd('C:/Users/1/Desktop/rlms')

# Loading the joint RLMS database
memory.limit(2e10)
dat_joined <- foreign::read.spss(file = "USER_RLMS-HSE_IND_1994_2018_v2.sav",
                                 use.value.labels = F,
                                 use.missings = T,
                                 to.data.frame = T)

# Changing . to _ in column names
colnames(dat_joined) = gsub("\\.", "_", colnames(dat_joined))

# Renaming the duplicated variables (K8_2 and K8_1), which appeared due to 
# the previous replacement in column names 
colnames(dat_joined)[which(colnames(dat_joined) == 'K8_2')[1]] <- 'K8_2_1'
colnames(dat_joined)[which(colnames(dat_joined) == 'K8_1')[1]] <- 'K8_1_1'

# Saving each column to a separate .csv file
# Create a folder called csv_tables in the working directory before that
i = 1
for (name in colnames(dat_joined)){
  write.csv(dat_joined[name], file = paste('csv_tables/', name, ".csv", sep=""))
  print(i)
  i = i + 1
}

### 
# End of file

