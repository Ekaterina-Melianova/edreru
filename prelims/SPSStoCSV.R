# SPSStoCSV.R
# This is an auxiliary file that writes each variable in RLMS database to .csv
# The resulting data will be imported to SQLite for the easiness of retrieval


library(sqldf)
library(XLConnectJars)

# Setting wd to Google Drive
setwd(paste0(normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"), "/Google Drive"))

# Loading the full base
memory.limit(1e10)
dat_joined <- foreign::read.spss(file="./SEABYTE/RLMS/rawdata/Joined_database/USER_RLMS-HSE_IND_1994_2017_v2.sav",
                                 use.value.labels = F,
                                 use.missings=TRUE,
                                 to.data.frame = TRUE)

# Changing . to _ in column names (SQlite treats . as parts of commands)
colnames(dat_joined) = gsub("\\.", "_", colnames(dat_joined))

# Renaming manually duplicates
which(colnames(dat_joined) == 'K8_2')
colnames(dat_joined)[2608] <- 'K8_2_1'
which(colnames(dat_joined) == 'K8_1')
colnames(dat_joined)[2607] <- 'K8_1_1'

# Writing each column to a separate .csv file
i = 1
for (name in colnames(dat_joined)){
  write.csv(dat_joined[name], file = paste('csv_tables/', name, ".csv", sep=""))
  print(i)
  i = i + 1
}


# The same procedure for the duplicates
i = 1
for (name in c('K8_1', 'K8_1_1', 'K8_2', 'K8_2_1')){
  write.csv(dat_joined[name], file = paste('csv_tables/', name, ".csv", sep=""))
  print(i)
  i = i + 1
}


