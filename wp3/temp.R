library(RSQLite)
library(sqldf)
library(DBI)

# Connecting with SQLite


db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")
dbListTables(db)


df_ <- selectFromSQL(c("J1", "J5A","J5B","H7_2", "YEAR", "J35_2Y", "J35_2M"))


temp1_ <- selectFromSQL(c("J41", "YEAR")) 
dbDisconnect(db) # I disconnect as the connection is no longer needed

db_list_tables(db)

talis <- dbConnect(RSQLite::SQLite(), "C:/Country/Bulgaria/Data/TALIS/sqlite/talis.sqlite")


# varlist
talis <- dbConnect(RSQLite::SQLite(), "C:/Country/Bulgaria/Data/TALIS/sqlite/talis.sqlite")
# Create Table
dbWriteTable(talis, "t_talis_18.varlabels", data.key.18,overwrite=TRUE)
dbDisconnect(talis)

dbListTables(db)
