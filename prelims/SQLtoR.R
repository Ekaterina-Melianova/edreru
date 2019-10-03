# SQLtoR.R
# The function in this script - selectFromSQL - is designed to simplify variable retrieval from the RLMS database.

library(sqldf)
library(XLConnectJars)


# Connect with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEABYTE/RLMS/sqlite/rlms.db")

# Modified cbind
cbind.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - 
                                                          nrow(x), ncol(x)))))
}

# Main function
selectFromSQL <- function(column_names=NULL, column_blocks=NULL, wave_number=NULL, dbname = "rlms.db"){
  "
  1. column_names - select specific column/s

  2. column_blocks - select specific block/s of columns. Available blocks:
      Bank services
      Children
      Daily activities
      Education
      Elections
      Employment
      Employment/finance (retrospective)
      Family
      Finance
      For women only
      Health assessment
      Identification variables
      Inequity issues
      Insurance
      Interviewer's remarks
      IT skills
      Law
      Living conditions
      Maternal capital
      Medical care
      Migration
      Military service
      Nationality issues
      Other
      Pension
      Personality assessment
      Politics
      Religion
      Safety/crimes
      Shopping
      Socio-demographics
      Sorces of news
      State services
      Transition period
      Traveling
      Trust

  3. wave_number - select specific wave/s in RLMS
  
  4. db_name - name of database in SQLite, by default rlms.db
      
  "
  
  # Add blocks to columns
  if (is.null(column_blocks) == FALSE){
    # Load table with blocks
    blocks_df <- sqldf('SELECT * from rlms_blocks', dbname = dbname)
    # Get columns from the selected blocks
    columns_from_blocks <- c()
    for (block in column_blocks){
      columns_from_blocks <- c(columns_from_blocks, blocks_df[blocks_df$column_block == block,]$column_names)
    }
    column_names <- c(column_names, columns_from_blocks)
  }
  
  # Add ids to the columns
  column_names <- unique(c("ID_W", "IDIND", "REDID_I", "ID_I", "ID_H", column_names))
  # Condition on a wave number 
  if (is.null(wave_number) == FALSE){
    if(length(wave_number) == 1){
      wave_condition <- paste('WHERE ID_W =', wave_number)
    } else {
      wave_condition <- paste('WHERE', paste(paste0('ID_W=', wave_number), collapse=' or '))
    }
  } else {
    wave_condition <- ""
  }
  
  # In case if the number of columns is 63 or larger (a default limitation of SQLite)
  if (length(column_names) >= 63){
    # Create a list with column chunks
    column_splits <- split(column_names, ceiling(seq_along(column_names)/63))
    # Add the columns by parts
    result_df <- data.frame()
    for (column_split in column_splits){
      if ('ID_W' %in% column_split == FALSE){
        column_split <- c('ID_W', column_split)
      }
      
      command_line <- paste(c("SELECT", paste(column_split, collapse=', '),
                              "FROM", paste(column_split, collapse=' NATURAL JOIN '),
                              wave_condition), collapse = ' ')
      cat('--- SQL command:', command_line, sep="\n")
      temp_df <- data.frame(sqldf(command_line, dbname = dbname))
      result_df <- data.frame(cbind.all(result_df, temp_df))
    }
    # Remove duplicates of the ID_W column
    result_df <- result_df[, -grep("ID_W.", colnames(result_df))]

  } else {
    command_line <- paste(c("SELECT", paste(column_names, collapse=', '),
                            "FROM", paste(column_names, collapse=' NATURAL JOIN '),
                            wave_condition), collapse = ' ')
    cat('--- SQL command:', command_line, sep="\n")
    result_df <- sqldf(command_line, dbname = dbname)
  }
  
  return(result_df)
}


# Examples
test_example_1 <- selectFromSQL(column_names='J193_2')
test_example_2 <- selectFromSQL(column_blocks='Religion')
test_example_3 <- selectFromSQL(column_names='J193_2', column_blocks='Religion', wave_number=5)
test_example_4 <- selectFromSQL(column_names=c('J193_2', 'J193_3'),
                                column_blocks = c('Religion', 'Safety/crimes', 'Socio-demographics'),
                                wave_number = c(5,10))
test_example_5 <- selectFromSQL(column_names=c('J193_2', 'J193_3'),
                                column_blocks = c('Religion', 'Safety/crimes', 'Socio-demographics'))


