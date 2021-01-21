# edreru_package.R

# These are functions we use often, in one place. 

######################################################################################
# Modified cbind #####################################################################
# We use this function as a sub-function in another user-defined function below, called "selectFromSQL"
# cbind.all cbinds the penultimate return from selectFromSQL to generate an R dataframe
# Ignore a "missing argument to function call" warning if your Rstudio IDE shows such a warning

cbind.all <- function (...){
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - 
                                                          nrow(x), ncol(x)))))
}

#######################################################################################
# SysMisFix function ##################################################################

# Defining functions to differentiate between "user-defined" missing values which are generated because 
# respondents did not answer a question they were supposed to answer, as different from
# "system-defined" missing values generated automatically from the questionnaire flow structure. 

# This is required when you use imputation to replace missing values - which only makes sense
# for user-defined missing values



## Convert alphanumeric or factor "NA" in the RLMS database to become R system NAs 
SysMisFix <- function(df){
  "SysMisFix changes categorical NA to missing values"
  temp <- df
  for (i in colnames(df)){
    temp[,i] <- mapvalues(df[,i], "NA", NA, warn_missing = F)
  }
  return(temp)
}

#######################################################################################
# UserMisFix function ##################################################################

UserMisFix <- function(df, na_range = 99999996:99999999){
  "UserMisFix labels user-defined missings as missing value "
  for (i in colnames(df)){
    if (is.character(df[,i]) == T){
      na_values(df[,i]) <- as.character(na_range)
    }
    else if (is.factor(df[,i]) == T){
      na_values(df[,i]) <- NULL
    }
    else if (!i %in% c("ID_W", "IDIND","YEAR","REDID_I","ID_I","ID_H")){
      na_values(df[,i]) <- na_range
    }
  }
  return(df)
} 


################################################################################################
#selectFromSQL function ########################################################################
# A function for variable selection
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

##############################################################################################
##############################################################################################
#Freq function -
# A function for calculating descriptive statistics: a slightly extended version of freq
# so that the resulting output is easier to comprehend 
options(scipen=999) # to supress scientific notation
Freq <- function(var){
  a <- descr::freq(var,plot=FALSE)
  z <- sum(is.na(var)) # I want to display later this number of NA's 
  z2 <- z+sum(!is.na(var)) # I want to display later this total number
  print(z2)
  print(class(z2))
  less <- dim(a)[1]-2  # Freq provides two rows of NA's and total I need to drop
  a <- a[1:less,]
  # Freq orders the row numbers alphanumerically 1,10,11..2,21 
  # I need to use package::numfrom to add leading zeros to single digit numbers
  row.names(a) <- numform::f_pad_zero(row.names(a))
  a2 <- cbind(a,row.names(a)) 
  # I can't order the matric by manipulating rownames directly so I introduce as an additional column
  # Then I order using that new column and drop the fourth column after ordering is completed.
  row.names(a2) <- as.numeric(as.character(a2[,4]))
  a2 <- a2[order(a2[,4]),] # order to form 1,2,3
  class(a2) <- "numeric" # revert deafult of alphanumeric elements when a2 was created, back to numeric
  a2 <- a2[,-4] # drop the column used for ordering, since they are now rownames
  print(a2) # In case interested in exact numeric values
    b <- deparse(substitute(var)) # I use deparse(substitute(var)) to access name 'var' and not the object itself
  barplot(a2[,1],main=paste(colnames(a)[1],"of",b),col="blue", 
          xlab=paste("Number of NAs is",z,"out of",z2,"total"),ylab="count") # barplot mimics plot in descr::freq
  axis(2,font=2,col="red") 
  axis(1,font=1,col="black",labels=FALSE)
}

### 
# End of file

