# rosstat2a.R

# adapted from rosstat1a.R by EM for new WP3

library(foreign)
library(plyr); library(dplyr)
library(gmodels)
library(lmtest)
library(sqldf)
library(XLConnectJars)
library(questionr)
library(labelled)
library(tidyr)
library(magrittr)
library(ggplot2)
library(data.table)
library(pbapply)
library(gridExtra)

##########################################################################################################

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT"
setwd(wd) 

######################################### Data ###########################################################

rst_18 <- read.spss(file="rosstat_18.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_17 <- read.spss(file="rosstat_17.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_16 <- read.spss(file="rosstat_16.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_15 <- read.spss(file="rosstat_15.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_14 <- read.spss(file="rosstat_14.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)

# Working directory
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp5"
setwd(wd) 

df_18 <- rst_18 %>% select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2018)
df_17 <- rst_17 %>% select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2017)
df_16 <- rst_16 %>% select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2016)
df_15 <- rst_15 %>% select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2015)
df_14 <- rst_14 %>% select(H00_02, H00_04, H01_00, H01_02, I01_10,
                           R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2014)

df_ <- rbind(df_14, df_15, df_16, df_17, df_18)

############################################################################################################

# Filtering age
table(df_$H01_02)
df <- df_[df_$H01_02 >= 18 & df_$H01_02 < 65,]

# Filtering employed
df <- df[!is.na(df$VZR_RAB),]
table(df$VZR_RAB)

# Education 
table(df$I01_10, df$YEAR)

# 4 categories:
# 0 - lower than secondary
# 1 - secondary 
# 2 - specialized / vocational
# 3 - higher and above

df$edu_4 <- car::recode(df$I01_10, "9=0; 7:8=1; 5:6=2; 1:4=3")
table(df$edu_4, df$YEAR)

# Filtering 3 education levels
df <- df[df$edu_4>0,]

# Education as factor
df$edu_4 <- factor(df$edu_4, levels=c(1,2,3),
                   labels=c("Secondary",
                            "Vocational",
                            "Higher"))

# Wage
df$wage <- df$R_DEN/12
aggregate(wage~YEAR, df, mean)

# Filtering wage > 0 
df <- df %>%
  filter(wage >0)

# Socio-demographics
# Gender
table(df$H01_01)
df$female[df$H01_01==2] <- 1
df$female[df$H01_01==1] <- 0
table(df$female)

# Experience (naive)
df$edu_yrs <- car::recode(df$I01_10, "1=20; 2=17; 3=16; 4=14; 5=12;
                            6=11; 7=11; 8=9")
df$exper <- df$H01_02 - df$edu_yrs - 6
df$exper <- ifelse(df$exper < 0, 0, df$exper)
summary(df$exper)
glimpse(df)

## generate regional excerpts 
## a list of dataframes by each of the 85 regions
a <- df %>% group_by(H00_02) %>% group_split()
#names(filelist) <- paste0(rep(LETTERS[1:1], each=38), rep(1:38, 1))
#sprintf("% 02d",5)
seq_region <- unique(df$H00_02) %>% sort()
# a vector of the 85 region numbers, from 01 to 99

# Now split the list a into AAxx where xx is region number and data frame
for(i in seq(seq_region)){
  assign(paste0("AA",seq_region[i]),   a[[i]])
}

# wage by age group 
AA01b <- AA01 %>% select(wage, H01_02) %>%  group_by(H01_02) %>% summarise(mwage=mean(wage))
  
plot(AA01b$H01_02,AA01b$mwage, type='b', col='red')
xspline(AA01b$H01_02,AA01b$mwage, shape=1)

for(i in seq(seq_region)){
  b <- get(paste0("AA",seq_region[i])) %>% 
    select(wage, H01_02) %>%  
          group_by(H01_02) %>% 
         summarise(mwage=mean(wage))
  assign(paste0("AB",seq_region[i]),b)
  }


pfun <- function(x){
  plot(x$H01_02,x$mwage,type='b',col='red') 
xspline(x$H01_02,x$mwage,shape=1)
print()
}

pfun(AB01)
# Future task - to print out series of all 85 graphs






