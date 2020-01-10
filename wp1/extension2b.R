# extension2b.R

# Sparklines for Weber

library(sparkTable)
library(rio)
library(reshape2)

# wd
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata")

# Editing

################ all
sixyrs_all <- import("6yrs_tbl_all.csv")
sixyrs_all <- as.data.frame(lapply(sixyrs_all, function(x) {substr(x, 3, nchar(x)-1)}))[c(2,4,6,8,9),]
colnames(sixyrs_all) <- c('Parameter', '1994', '1998', '2003', '2006', '2012', '2018')
sixyrs_all$Parameter <- c('lnW', 'bk', 'delta', 'alpha', 'Sample size')
rownames(sixyrs_all) <- NULL
# Rounding
sixyrs_all[-1] <- lapply(sixyrs_all[-1], as.character)
sixyrs_all[-1] <- lapply(sixyrs_all[-1], as.numeric)
sixyrs_all[] <- lapply(sixyrs_all, function(x) if(is.numeric(x)) round(x, 4) else x)

# delta and alpha are not significant in 2003 and 2006 --> 0
#sixyrs_all$`2003` <- c(0, 0)
#sixyrs_all$`2006` <- c(0, 0)

################ Females
sixyrs_f <- import("6yrs_tbl_f.csv")
sixyrs_f <- as.data.frame(lapply(sixyrs_f, function(x) {substr(x, 3, nchar(x)-1)}))[c(6, 8),]
colnames(sixyrs_f) <- c('Parameter', '1994', '1998', '2003', '2006', '2012', '2018')
sixyrs_f$Parameter <- c('delta', 'alpha')
rownames(sixyrs_f) <- NULL
# Rounding
sixyrs_f[-1] <- lapply(sixyrs_f[-1], as.character)
sixyrs_f[-1] <- lapply(sixyrs_f[-1], as.numeric)
sixyrs_f[] <- lapply(sixyrs_f, function(x) if(is.numeric(x)) round(x, 4) else x)

# delta and alpha are not significant in 2006 --> 0
sixyrs_f$`2006` <- c(0, 0)

################ Males
sixyrs_m <- import("6yrs_tbl_m.csv")
sixyrs_m <- as.data.frame(lapply(sixyrs_m, function(x) {substr(x, 3, nchar(x)-1)}))[c(6, 8),]
colnames(sixyrs_m) <- c('Parameter', '1994', '1998', '2003', '2006', '2012', '2018')
sixyrs_m$Parameter <- c('delta', 'alpha')
rownames(sixyrs_m) <- NULL
# Rounding
sixyrs_m[-1] <- lapply(sixyrs_m[-1], as.character)
sixyrs_m[-1] <- lapply(sixyrs_m[-1], as.numeric)
sixyrs_m[] <- lapply(sixyrs_m, function(x) if(is.numeric(x)) round(x, 4) else x)

# delta and alpha are not significant in 2003 and 2006 --> 0
sixyrs_m$`2003` <- c(0, 0)
sixyrs_m$`2006` <- c(0, 0)

# Latex
xtable::xtable(sixyrs_all)
xtable::xtable(sixyrs_f)
xtable::xtable(sixyrs_m)

##################################### Sparklines ############################################

# Long format
(sixyrs_all_ <- melt(sixyrs_all, id.vars="Parameter", variable.name = "time"))
(sixyrs_f_ <- melt(sixyrs_f, id.vars="Parameter", variable.name = "time"))
(sixyrs_m_ <- melt(sixyrs_m, id.vars="Parameter", variable.name = "time"))

## sparkTable needs two parameters in addition to data - content and VarType
content <- list(
  function(x) {round(mean(x), 2)}, newSparkLine())
names(content) <- paste('column', 1:2, sep='')
varType <- rep('value', 2)

# sparkTable needs to run function reshapeExt on long form of data
sixyrs_all_ <- sixyrs_all_[, c('Parameter', 'value', 'time')]
(spk_all <- reshapeExt(sixyrs_all_, varying = list(2))) 

sixyrs_f_ <- sixyrs_f_[, c('Parameter', 'value', 'time')]
(spk_f <- reshapeExt(sixyrs_f_, varying = list(2))) 

sixyrs_m_ <- sixyrs_m_[, c('Parameter', 'value', 'time')]
(spk_m <- reshapeExt(sixyrs_m_, varying = list(2))) 

# reshapeExt at up the time and added an id, I just fix that
spk_all$time <- sixyrs_all_$time
spk_all$id <- NULL

spk_f$time <- sixyrs_f_$time
spk_f$id <- NULL

spk_m$time <- sixyrs_m_$time
spk_m$id <- NULL

# Generate the sparkTable
spk_all_out <- newSparkTable(spk_all, content, varType)
spk_f_out <- newSparkTable(spk_f, content, varType)
spk_m_out <- newSparkTable(spk_m, content, varType)

# Latex
sparkTable::export(spk_all_out, outputType="tex", 
       filename="C:/Country/Russia/Data/SEASHELL/SEABYTE/Edreru/wp1/sparklines/Weber_sprk_all",
       graphNames="C:/Country/Russia/Data/SEASHELL/SEABYTE/Edreru/wp1/sparklines/Weber_sprk_all")

sparkTable::export(spk_f_out, outputType="tex", 
       filename="C:/Country/Russia/Data/SEASHELL/SEABYTE/Edreru/wp1/sparklines/Weber_sprk_f",
       graphNames="C:/Country/Russia/Data/SEASHELL/SEABYTE/Edreru/wp1/sparklines/Weber_sprk_f")

sparkTable::export(spk_m_out, outputType="tex", 
       filename="C:/Country/Russia/Data/SEASHELL/SEABYTE/Edreru/wp1/sparklines/Weber_sprk_m",
       graphNames="C:/Country/Russia/Data/SEASHELL/SEABYTE/Edreru/wp1/sparklines/Weber_sprk_m")







