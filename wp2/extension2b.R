# extension2b.R

# Sparklines for Weber

library(sparkTable)
library(reshape2)

# wd
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/Stata")

# Editing

################ all
sixyrs_all <- read.csv("6yrs_tbl_all.csv")
for (i in 1:ncol(sixyrs_all)){
  sixyrs_all[,i]<- as.character(sixyrs_all[,i])
}

sixyrs_all <- as.data.frame(lapply(sixyrs_all, function(x) {substr(x, 2, nchar(x))}))[c(2,3,5,6,8,9,11,12,13),]
colnames(sixyrs_all) <- c('Parameter', '1994', '1998', '2003', '2006', '2012', '2018')
sixyrs_all$Parameter <- c('lnW', "", 'bk', "", 'delta', "", 'alpha', "", 'Sample size')
rownames(sixyrs_all) <- NULL

# Rounding
#sixyrs_all[] <- lapply(sixyrs_all, function(x) if(is.numeric(x)) round(x, 4) else x)

################ Females
sixyrs_f <- read.csv("6yrs_tbl_f.csv")
for (i in 1:ncol(sixyrs_f)){
  sixyrs_f[,i]<- as.character(sixyrs_f[,i])
}

sixyrs_f <- as.data.frame(lapply(sixyrs_f, function(x) {substr(x, 2, nchar(x))}))[c(2,3,5,6,8,9,11,12,13),]
colnames(sixyrs_f) <- c('Parameter', '1994', '1998', '2003', '2006', '2012', '2018')
sixyrs_f$Parameter <- c('lnW', "", 'bk', "", 'delta', "", 'alpha', "", 'Sample size')
rownames(sixyrs_f) <- NULL

################ Males
sixyrs_m <- read.csv("6yrs_tbl_m.csv")
for (i in 1:ncol(sixyrs_m)){
  sixyrs_m[,i]<- as.character(sixyrs_m[,i])
}

sixyrs_m <- as.data.frame(lapply(sixyrs_m, function(x) {substr(x, 2, nchar(x))}))[c(2,3,5,6,8,9,11,12,13),]
colnames(sixyrs_m) <- c('Parameter', '1994', '1998', '2003', '2006', '2012', '2018')
sixyrs_m$Parameter <- c('lnW', "", 'bk', "", 'delta', "", 'alpha', "", 'Sample size')
rownames(sixyrs_m) <- NULL

# Latex
xtable::xtable(sixyrs_all)
xtable::xtable(sixyrs_f)
xtable::xtable(sixyrs_m)

# only delta and alpha for sparks
sixyrs_all_ <- sixyrs_all[c(5,7),]
sixyrs_f_ <- sixyrs_f[c(5,7),]
sixyrs_m_ <- sixyrs_m[c(5,7),]

##################################### Sparklines ############################################

# Long format
(sixyrs_all_ <- melt(sixyrs_all_, id.vars="Parameter", variable.name = "time"))
(sixyrs_f_ <- melt(sixyrs_f_, id.vars="Parameter", variable.name = "time"))
(sixyrs_m_ <- melt(sixyrs_m_, id.vars="Parameter", variable.name = "time"))

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







