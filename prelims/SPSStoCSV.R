# SPSStoCSV.R
# This is an auxiliary file that writes each variable in RLMS database to .csv
# The resulting data will be imported to SQLite for the easiness of retrieval


library(sqldf)
library(XLConnectJars)

# Setting wd to Google Drive
setwd(paste0(normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"), "/Google Drive"))

# Loading the full base
memory.limit(1e10)
dat_joined <- foreign::read.spss(file="USER_RLMS-HSE_IND_1994_2018_v2.sav",
                                 use.value.labels = F,
                                 use.missings=F,
                                 to.data.frame = TRUE)

# Renaming manually duplicates
colnames(dat_joined)[which(colnames(dat_joined) == 'K8_2')] <- 'K8_2_1'
colnames(dat_joined)[which(colnames(dat_joined) == 'K8_1')] <- 'K8_1_1'


# Changing . to _ in column names (SQlite treats . as parts of commands)
colnames(dat_joined) = gsub("\\.", "_", colnames(dat_joined))


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


i = 1
for (name in c('K8_1', 'K8_1_1', 'K8_2', 'K8_2_1')){
  write.csv(dat_joined[name], file = paste('csv_tables/', name, ".csv", sep=""))
  print(i)
  i = i + 1
}


