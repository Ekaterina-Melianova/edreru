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
library(nnet)
library(splitstackshape)
library(stringi)
library(stringr)
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

# compute mean and sd wage by age group 

for(i in seq(seq_region)){
  b <- get(paste0("AA",seq_region[i])) %>% 
    select(wage, H01_02) %>%  
          group_by(H01_02) %>% 
         summarise(mwage=mean(wage),sdwage=sd(wage))
  assign(paste0("AB",seq_region[i]),b)
  }

# Ordinary spline plot
pfun <- function(x){
  plot(x$H01_02,x$mwage,type='b',col='red') 
xspline(x$H01_02,x$mwage,shape=1)
}


###############
## Below I do for one region, this would need to be automated
## and replicated for all regions

## There is mismatch between Rosstat 2 digit codes for regions
## and Artëm's College data 

## 70 from Artëm is Tomsk Oblast, 69 in Rosstat from
## /wp1/rgvars.xlsx

## In naming, the Rosstat/rgvars dominates.


pfun(AB69)

# smooth spline
ab69_ <- smooth.spline(AB69$H01_02, y=AB69$mwage, w = NULL, df=3, spar = NULL,
              cv = FALSE, all.knots = FALSE, nknots = NULL,
              keep.data = TRUE, df.offset = 0, penalty = 1,
              control.spar = list())
plot(ab69_,type='l', col='red')


#


# Pick up 20 random numbers without replacement
sam_69 <- AB69 %>% filter(H01_02>25) %>%  sample_n(20) %>%
  arrange(H01_02) %>% 
  pivot_wider(names_from = H01_02, values_from = c(mwage,sdwage),
              names_prefix = "") %>% 
         expandRows(sam_69,count.is.col=FALSE,count=23)
# a bit redundant, but easy

# Actually nothing served by selecting randomly at this stage
# only after merging with 4 actual values
sam_69 <- AB69 %>% 
  arrange(H01_02) %>% 
  pivot_wider(names_from = H01_02, values_from = c(mwage,sdwage),
              names_prefix = "") %>% 
  expandRows(sam_69,count.is.col=FALSE,count=23)
# a bit redundant, but easy


# on second thoughts, sd is not going to be of much use, we are only
# going to jitter the numbers, no need for sd
sam_69 <- sam_69 %>% select(1:47)


# I retrieve the colleges from FD 70 of Artëm's data which is 69
# May need to fix region code for matching




df69_ <- df_col2 %>% filter(region_code==70)
df69 <- cbind(df69_,sam_69) 
df69$graduate_age <- round(df69$graduate_age)

# from graduate age to graduate age + 3 we have original data
# 2016, 2017, 2018, 2019 

# after this we have extrapolations 
# mean for the age + or - 10% of standard deviation for that age
# + or - random fluctuations of 0 to 5 % of standard deviation

table(df69$graduate_age)

# Create 5 additional columns and assign NA to ot
df69[,158:162] <- NA
df69[,163:167] <- NA

# select 5 from the vector of names
# create an empty dataframe with 23 rows and 5 columns
atemp <- data.frame(matrix(NA,nrow=23, ncol=5))


# now populate this dataframe with 5 specific random mwage points
for(i in 1:nrow(df69)){
atemp[i,] <-  sort(sample(colnames(df69[(111+df69[i,17]-14):157]),5))
}

atemp


#df69[1,158] <- df69[1,vec1[1,1]]
#df69[1,163] <- substr(atemp[1,1],7,8)

# now populate the columns 158 t0 162 with 5 mwages
# and columns 163 to 167 with corresponding 5 ages 

for(i in 1:nrow(df69)){
  vec1 <- noquote(atemp[i,])
  for(j in 1:5){
        df69[i,157+j] <- df69[i,vec1[1,j]]
        df69[i,162+j] <- substr(atemp[i,j],7,8)
   }
    }


# add 4 ages of graduate_age and 3 more
df69[,168:170] <- NA

for(i in 1:nrow(df69)){
  for(k in 1:3){
  df69[i,167+k] <- df69[i,17]+k
  }
}


# now almost get the dataframe from which you want to make spline

ndf69_ <- df69 %>% select(inn,graduate_age,V168:V170,V163:V168)

#add four NA colums

ndf69_[,11:14] <- NA


ndf69_[,11:14] <- df69[,13:16]

# add the 4 graduate_age corresponding columns from the synthetics data

# create another temporary dataframe
# 4 columns of graduate_age to +3


(btemp <- data.frame(matrix(NA,nrow=23, ncol=4)))

# populate this dataframe with the names of the 
# mwagexx constructs we need

for(i in 1:nrow(btemp)){
  for(k in 1:4){
    btemp[i,k] <- paste0("mwage_",df69[i,17]+k-1)   
  }
}

btemp


# Now we are back to add the 4 wage  variables to ndf69_

ndf69_[,15:18] <- NA



# now populate the columns 158 t0 162 with 5 mwages
# and columns 163 to 167 with corresponding 5 ages 

for(i in 1:nrow(ndf69_)){
  vec1 <- noquote(btemp[i,])
    for(j in 15:18){
             ndf69_[i,j] <- round(df69[i,vec1[1,j-14]])
      }
}


# and add the five wage variables related to atemp

ndf69_[19:23] <- NA

for(i in 1:nrow(ndf69_)){
  vec1 <- noquote(atemp[i,])
  for(j in 19:23){
    ndf69_[i,j] <- round(df69[i,vec1[1,j-18]])
  }
}

# now name the columns of ndf69_ to make it a bit
# easier for the next set of calculations
n1 <- c("inn", "gage1","gage2","gage3","gage4")
n2 <- c("rage1","rage2","rage3","rage4","rage5")
n3 <- c("wage1","wage2","wage3","wage4")
n4 <- c("mwage1","mwage2","mwage3","mwage4")
n5 <- c("rwage1","rwage2","rwage3","rwage4","rwage5")


colnames(ndf69_) <- c(n1,n2,n3,n4,n5)

##
# now we use the ratio of rwage to mwage1 and so on 5 times
# and use the ratio on wage1 to wage4 to get ewage1 to ewage5
# my five extrapolated wages of future

ndf69 <- ndf69_ %>% mutate(
            rw1=rwage1/mwage1, ewage1=round(wage1*rw1),
            rw2=rwage2/mwage2, ewage2=round(wage2*rw1),
            rw3=rwage3/mwage3, ewage3=round(wage3*rw1),
            rw4=rwage4/mwage4, ewage4=round(wage4*rw1),
            rw5=rwage5/mwage1, ewage5=round(wage1*rw1),
) %>%  select("gage1","gage2","gage3","gage4",
             "rage1","rage2","rage3","rage4","rage5",
             "wage1","wage2","wage3","wage4",
             "ewage1","ewage2","ewage3","ewage4","ewage5")


par(mfrow=c(3,4))
for(i in 1:12){
  x <- ndf69[i,c("gage1","gage2","gage3","gage4",
                  "rage1","rage2","rage3","rage4","rage5")]
  y <- ndf69[12,c("wage1","wage2","wage3","wage4",
                  "ewage1","ewage2","ewage3","ewage4","ewage5")]
  s69_1 <- smooth.spline(x, y, w = NULL, df=3, spar = NULL,
                          cv = FALSE, all.knots = FALSE, nknots = NULL,
                          keep.data = TRUE, df.offset = 0, penalty = 1,
                          control.spar = list())
  plot(s69_1,type='l', col='blue')
    }


for(i in 13:23){
  x <- ndf69[i,c("gage1","gage2","gage3","gage4",
                 "rage1","rage2","rage3","rage4","rage5")]
  y <- ndf69[12,c("wage1","wage2","wage3","wage4",
                  "ewage1","ewage2","ewage3","ewage4","ewage5")]
  s69_1 <- smooth.spline(x, y, w = NULL, df=3, spar = NULL,
                         cv = FALSE, all.knots = FALSE, nknots = NULL,
                         keep.data = TRUE, df.offset = 0, penalty = 1,
                         control.spar = list())
  plot(s69_1,type='l', col='blue')
}





