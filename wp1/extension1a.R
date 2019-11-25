# extension1a.R
# WP1 extension along lines of Neuman-Weiss 1995.

library(plyr); library(dplyr)
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
library(psych)
library(stringi)
library(sjPlot)
library(sjmisc)
library(segregation)
library(reshape2)

# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Specify the default working directory for this script
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1")

# Load data generated and saved in mincer2a.R file with RLMS 1994-2018 cleaned and cooked well
df_mincer <- readRDS("df_mincer.rds")

## I want to bring in some other variables from the rawdata sqlite database.
## The additional variables are
# J41 - Industry or Sector of employment in RLMS
## I neeed the function selectFromSQL defined in mincer1a.R
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite")

# Connecting with SQLite
db <- dbConnect(SQLite(), dbname="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/RLMS/sqlite/rlms.db")
temp1_ <- selectFromSQL(c("J4_1", "YEAR")) 
dbDisconnect(db) # I disconnect as the connection is no longer needed

############################################################################################
# I merge the variable I need and drop the other classifying variable
temp1 <- df_mincer %>%
  left_join(temp1_, by = c("YEAR", "IDIND")) %>% select(-ID_I,-ID_H,-ID_W,-REDID_I)

rm(temp1_) # don't need this anymore
# Fixing system and user-defined missings in the RLMS database

temp1 <- SysMisFix(temp1) # determining system missings
temp1 <- UserMisFix(temp1) # labelling user-defined missings
#

# convert large 9999 numbers to missing 
# Interrogative - is this a default for all variables or every variable 
# has different set of numerically coded missing values

temp1$J4_1[temp1$J4_1==99999996] <- NA
temp1$J4_1[temp1$J4_1==99999997] <- NA
temp1$J4_1[temp1$J4_1==99999998] <- NA
temp1$J4_1[temp1$J4_1==99999999] <- NA

FreqSP(temp1$J4_1)  
############################################################################################
# Move the wd back to project root
setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1")


##########
# I will calculate the proportion of women in labor force in 2018
temp218_ <- temp1 %>% filter(YEAR==2018) # 6,111 observations

# By Industry/Sector J4_1 ############********************
# How many NAs on J4_1
(sum(is.na(temp218_$J4_1)))
# 22 

# 6011-22 = 6089 observations left below from filtering NAs on J4_1

t2_ <- temp218_ %>% filter(!is.na(J4_1)) %>% group_by(J4_1) %>% 
  summarize(tfem=sum(female),tall=n()) %>%  arrange(desc(tall))

(sum(t2_$tfem))/(sum(t2_$tall)) # 0.5425 
# I take as 54% - more than 10% or more than 64% regarded as female dominated industry
#               - less than 10% or less than 44% regarded as male dominated
# those in the middle are neutral

# Visual observation of t2_ tells me there are five industries in single digits,
# I will combine them into other 22, 28, 29, 31, 32 and give it value 19
# which was an original "others" value in the RLMS Questionnaire in 26th Round

t2_ <- temp218_ %>% filter(!is.na(J4_1)) %>% group_by(J4_1) %>% 
  summarize(tfem=sum(female),tall=n()) %>%  
    mutate(J4_1=replace(J4_1,J4_1%in%c(22,28,29,31,32),19)) %>% arrange(J4_1) %>% 
  group_by(J4_1) %>% summarize(tfem=sum(tfem),tall=sum(tall)) %>% 
   mutate(J4_1=as.numeric(J4_1)) %>% mutate(pfem=tfem/tall) %>% 
   mutate(pfem=round(pfem,4)*100,pmal=100-pfem) 

t2_$J4_1 <- as.numeric(t2_$J4_1)

(sum(t2_$tfem)) # 3303
(sum(t2_$tall)) # 6089

t2_$indcat <- NA  # to avoid multiple warnings

t2_$indcat[t2_$pfem>64] <- "Ind_F"
t2_$indcat[t2_$pfem<44] <- "Ind_M"
t2_$indcat[t2_$pfem>=44&t2_$pfem<=64] <- "Ind_N"


# Defining the values 
t2_$ilab = NA

t2_$ilab[t2_$J4_1==1] <- "Light industry, Food industry"
t2_$ilab[t2_$J4_1==2] <- "Civil Machine Construction"
t2_$ilab[t2_$J4_1==3] <- "Miltary Industrial Complex"
t2_$ilab[t2_$J4_1==4] <- "Oil and Gas Industry"

t2_$ilab[t2_$J4_1==5] <- "Other Heavy Industry"
t2_$ilab[t2_$J4_1==6] <- "Construction"
t2_$ilab[t2_$J4_1==7] <- "Transportation, Communication"
t2_$ilab[t2_$J4_1==8] <- "Agriculture"

t2_$ilab[t2_$J4_1==9] <- "Government and Public Administration"
t2_$ilab[t2_$J4_1==10]<- "Education"
t2_$ilab[t2_$J4_1==11]<- "Science, Culture"
t2_$ilab[t2_$J4_1==12]<- "Public Health"

t2_$ilab[t2_$J4_1==13]<- "Army, Internal Security"
t2_$ilab[t2_$J4_1==14]<- "Trade, Consumer Services"
t2_$ilab[t2_$J4_1==15]<- "Finance"
t2_$ilab[t2_$J4_1==16]<- "Energy or Power Industry"

t2_$ilab[t2_$J4_1==17]<- "Housing and Community Services"
t2_$ilab[t2_$J4_1==18]<- "Real Estate Operations"
t2_$ilab[t2_$J4_1==19]<- "Other"
t2_$ilab[t2_$J4_1==20]<- "Social Services"

t2_$ilab[t2_$J4_1==21]<- "Jurisprudence"
t2_$ilab[t2_$J4_1==23]<- "Chemical Industry"
t2_$ilab[t2_$J4_1==24]<- "Wood, Timber, Forestry"
t2_$ilab[t2_$J4_1==25]<- "Sports, Tourism,Entertainment"

t2_$ilab[t2_$J4_1==26]<- "General Public Services"
t2_$ilab[t2_$J4_1==27]<- "Information Technology"
t2_$ilab[t2_$J4_1==30]<- "Mass Media, Telecommunications"

## Table
t2t_ <- t2_ %>% select(ilab,tfem,pfem,tall) %>% 
  transmute(ilab=ilab,tfem=as.character(tfem),pfem=numform::f_percent(pfem),tall=tall) %>%
  arrange(desc(pfem)) 

# Convert table into latex format
xtable::xtable(t2t_)
rm(t2t_)


# Keep only variables needed for graphing and introduce rank
t2_ <- t2_ %>% arrange(desc(pfem)) %>% select(J4_1,pfem,pmal) %>% 
  mutate(rankf=rank(desc(pfem)))

# convert into long form for ggplot2
t2b_ <- reshape2::melt(t2_,id.vars=(c("J4_1","rankf")), value.name="percentage", variable.name="gender")

# Now for graph
ggplot(t2b_,aes(x=reorder(as.factor(J4_1),rankf),y=percentage,fill=gender))+
  geom_bar(stat="identity")+
  geom_hline(yintercept=54,color="blue",lty="solid")+
  geom_hline(yintercept=44,color="black",lty=2)+
  geom_hline(yintercept=64,color="black",lty=2)+
  xlab("Industry Sectors") +
  ylab("Percentage") +
  theme(legend.position ="NONE")
  
ggsave("gen_ind18.png", width = 7.5, height = 4,
       units = "in")



## By single occupational digit#################
temp218_ <- temp1 %>% filter(YEAR==2018) # 6,111 observations
temp218_ <- temp218_ %>% mutate(isco_1=substr(occup,1,1))

t2_ <- temp218_ %>% group_by(isco_1) %>% 
  summarize(tfem=sum(female),tall=n()) %>%  arrange(desc(tall))

# manual observation indicates 
# 6 individuals with 06 (farm and fishery) and 5 individuals with 00 (armed forces)
# otherwise lowest n is 339

# From 6111 to 6100 observations
t2_ <- temp218_ %>% filter(isco_1!=6 & isco_1!=0) %>% group_by(isco_1) %>% 
  summarize(tfem=sum(female),tall=n()) %>%  arrange(desc(tall))

(sum(t2_$tfem))/(sum(t2_$tall)) # 0.5423 


t2_ <- temp218_ %>% filter(isco_1!=6 & isco_1!=0) %>% group_by(isco_1) %>% 
  summarize(tfem=sum(female),tall=n()) %>%  
  mutate(pfem=tfem/tall) %>%  mutate(pfem=round(pfem,4)*100,pmal=100-pfem) 

t2_$indcat <- NA  # to avoid multiple warnings

t2_$indcat[t2_$pfem>64] <- "Ind_F"
t2_$indcat[t2_$pfem<44] <- "Ind_M"
t2_$indcat[t2_$pfem>=44&t2_$pfem<=64] <- "Ind_N"

# Keep only variables needed for graphing and introduce rank
t2_ <- t2_ %>% arrange(desc(pfem)) %>% select(isco_1,pfem,pmal) %>% 
  mutate(rankf=rank(desc(pfem)))


# convert into long form for ggplot2
t2b_ <- reshape2::melt(t2_,id.vars=(c("isco_1","rankf")),
                       value.name="percentage", variable.name="gender")

# Generate dataframe for geom_text
t2c_ <- t2_ %>% select(isco_1,pfem,rankf) %>% 
  transmute(isco_1=isco_1,percentage=pfem,rankf=rankf)

t2c_$ilabel <- NA
t2c_$ilabel[t2c_$isco_1==1] <- "                Managers"
t2c_$ilabel[t2c_$isco_1==2] <- "Professionals"
t2c_$ilabel[t2c_$isco_1==3] <- "   Technicians"

t2c_$ilabel[t2c_$isco_1==4] <- "Clerical"
t2c_$ilabel[t2c_$isco_1==5] <- "     Services"
t2c_$ilabel[t2c_$isco_1==7] <- "Crafts/Trades"

t2c_$ilabel[t2c_$isco_1==8] <- "Machine Operators"
t2c_$ilabel[t2c_$isco_1==9] <- "     Elementary Ocups."


t2c_$gender <- NA


# Now for graph
# We wont use this as it is not very intuitive
ggplot(t2b_,aes(x=reorder(as.factor(isco_1),rankf),y=percentage,fill=gender))+
  geom_bar(stat="identity")+
  geom_hline(yintercept=54,color="blue",lty="solid")+
  geom_hline(yintercept=44,color="black",lty=2)+
  geom_hline(yintercept=64,color="black",lty=2)+
  geom_text(data=t2c_, aes(x=isco_1, y=percentage, label = ilabel, 
                          hjust=0,vjust=0, angle = 90))+
  xlab("occupations") +
  ylab("Percentage") +
  theme(legend.position ="NONE")


## By double digit occupational digit#################
temp218_ <- temp1 %>% filter(YEAR==2018) # 6,111 observations
temp218_ <- temp218_ %>% mutate(isco_2=substr(occup,1,2))

## I want to see how many 2 digits are more than n=30 and combine others
## Visual inspection shows 20 or more except for
## 0,35, 61, 62, 95 

t2_ <- temp218_ %>% 
  mutate(isco_2=replace(isco_2,isco_2%in%c(0,35,61,62,95),66)) %>% arrange(isco_2) %>%   
  group_by(isco_2) %>% 
  summarize(tfem=sum(female),tall=n()) %>% arrange(desc(tall))

(sum(t2_$tfem))/(sum(t2_$tall)) # 0.5420

t2_ <- temp218_ %>% 
  mutate(isco_2=replace(isco_2,isco_2%in%c(0,35,61,62,95),66)) %>% arrange(isco_2) %>%   
  group_by(isco_2) %>% 
  summarize(tfem=sum(female),tall=n()) %>% arrange(desc(tall)) %>%
   mutate(pfem=tfem/tall) %>%  mutate(pfem=round(pfem,4)*100,pmal=100-pfem) %>% arrange(desc(pfem))


# keep for later merging
t2 <- t2_ 
t2$ocpcat18 <- NA
t2$ocpcat18[t2$pfem<44] <- "occmale"
t2$ocpcat18[t2$pfem>=44&t2$pfem<=64] <- "occneut"
t2$ocpcat18[t2$pfem>64] <- "occfemale"

t2 <- t2 %>% select(isco_2,ocpcat18)

# Defining the values 
t2_$ilab = NA

# Defining the values 
t2_$ilab = NA


t2_$ilab[t2_$isco_2==11]	<- "Chief Executives, Senior Officials and Legislators"
t2_$ilab[t2_$isco_2==12]	<- "Administrative and Commercial Managers"
t2_$ilab[t2_$isco_2==13]	<- "Production and Specialized Services Managers"
t2_$ilab[t2_$isco_2==14]	<- "Hospitality, Retail and Other Services Managers"

t2_$ilab[t2_$isco_2==21]	<- "Science and Engineering Professionals"
t2_$ilab[t2_$isco_2==22]	<- "Health Professionals"
t2_$ilab[t2_$isco_2==23]	<- "Teaching Professionals"
t2_$ilab[t2_$isco_2==24]	<- "Business and Administration Professionals"
t2_$ilab[t2_$isco_2==25]	<- "Information and Communications Technology Professionals"
t2_$ilab[t2_$isco_2==26]	<- "Legal, Social and Cultural Professionals"


t2_$ilab[t2_$isco_2==31] 	<- "Science and Engineering Associate Professionals"
t2_$ilab[t2_$isco_2==32]	<- "Health Associate Professionals"
t2_$ilab[t2_$isco_2==33]	<- "Business and Administration Associate Professionals"
t2_$ilab[t2_$isco_2==34]	<- "Legal, Social, Cultural and Related Associate Professionals"
t2_$ilab[t2_$isco_2==35]	<- "Information and Communications Technicians"

t2_$ilab[t2_$isco_2==41]	<- "General and Keyboard Clerks"
t2_$ilab[t2_$isco_2==42]	<- "Customer Services Clerks"
t2_$ilab[t2_$isco_2==43]	<- "Numerical and Material Recording Clerks"
t2_$ilab[t2_$isco_2==44]	<- "Other Clerical Support Workers"

t2_$ilab[t2_$isco_2==51]	<-"Personal Services Workers"
t2_$ilab[t2_$isco_2==52]	<-"Sales Workers"
t2_$ilab[t2_$isco_2==53]	<-"Personal Care Workers"
t2_$ilab[t2_$isco_2==54]	<-"Protective Services Workers"

t2_$ilab[t2_$isco_2==66]  <-"Miscellaneous non-ISCO"

t2_$ilab[t2_$isco_2==71]	<-"Building and Related Trades Workers (excluding Electricians)"
t2_$ilab[t2_$isco_2==72]	<-"Metal, Machinery and Related Trades Workers"
t2_$ilab[t2_$isco_2==73]	<-"Handicraft and Printing Workers"
t2_$ilab[t2_$isco_2==74]	<-"Electrical and Electronic Trades Workers"
t2_$ilab[t2_$isco_2==75]	<-"Food Processing, Woodworking, Garment and Other Craft and Related Trades Workers"

t2_$ilab[t2_$isco_2==81]	<-"Stationary Plant and Machine Operators"
t2_$ilab[t2_$isco_2==82]	<-"Assemblers"
t2_$ilab[t2_$isco_2==83]	<-"Drivers and Mobile Plant Operators"

t2_$ilab[t2_$isco_2==91]	<-"Cleaners and Helpers"
t2_$ilab[t2_$isco_2==92]	<-"Agricultural, Forestry and Fishery Labourers"
t2_$ilab[t2_$isco_2==93]	<-"Labourers in Mining, Construction, Manufacturing and Transport"
t2_$ilab[t2_$isco_2==94]	<-"Food Preparation Assistants"
t2_$ilab[t2_$isco_2==95]	<-"Street and Related Sales and Services Workers"
t2_$ilab[t2_$isco_2==96]	<-"Refuse Workers and Other Elementary Workers" 


## Table
t2t_ <- t2_ %>% select(ilab,tfem,pfem,tall) %>% arrange(desc(pfem)) %>%
  transmute(ilab=ilab,tfem=as.character(tfem),
            pfem=numform::f_percent(pfem),tall=tall) 
# Convert table into latex format
xtable::xtable(t2t_)
rm(t2t_)

# Keep only variables needed for graphing and introduce rank
t2_ <- t2_ %>% arrange(desc(pfem)) %>% select(isco_2,pfem,pmal) %>% 
  mutate(rankf=rank(desc(pfem)))


# convert into long form for ggplot2
t2b_ <- reshape2::melt(t2_,id.vars=(c("isco_2","rankf")),
                       value.name="percentage", variable.name="gender")


# Now for graph
ggplot(t2b_,aes(x=reorder(as.factor(isco_2),rankf),y=percentage,fill=gender))+
  geom_bar(stat="identity")+
  geom_hline(yintercept=54,color="blue",lty="solid")+
  geom_hline(yintercept=44,color="black",lty=2)+
  geom_hline(yintercept=64,color="black",lty=2)+
  xlab("Occupations") +
  ylab("Percentage") +
  theme(legend.position ="NONE")

ggsave("gen_occ18.png", width = 7.5, height = 4,
       units = "in")

## Later we could explore 4 digit ISCO and J4_1/ISCO 2 digit combinations
## But first we take 2 digit ISCO all the way


# Examine distribution of occupations
temp2_ <- temp1 %>% group_by(occup) %>% mutate(nocup=n_distinct(IDIND)) %>% select(occup,nocup) %>% distinct() %>%
           arrange(desc(nocup))

# Filter top 5 to test
temp2_ <- temp1 %>% filter(occup==5223|occup==8332|occup==5414|occup==3313|occup==9112) %>% filter(!is.na(J4_1))

temp2a_=temp2_ %>% filter(occup==5223) %>% group_by(J4_1) %>% select(IDIND,J4_1,occup,female) %>% distinct()
temp2b_=temp2_ %>% filter(occup==8332) %>% group_by(J4_1) %>% select(IDIND,J4_1,occup,female) %>% distinct()
temp2c_=temp2_ %>% filter(occup==5414) %>% group_by(J4_1) %>% select(IDIND,J4_1,occup,female) %>% distinct()
temp2d_=temp2_ %>% filter(occup==3313) %>% group_by(J4_1) %>% select(IDIND,J4_1,occup,female) %>% distinct()
temp2e_=temp2_ %>% filter(occup==9112) %>% group_by(J4_1) %>% select(IDIND,J4_1,occup,female) %>% distinct()


# Now calculate Karmel-Machlachlan by occupation 
(seg1_ <- segregation::mutual_total(data=temp2a_,group="J4_1",unit="female"))
(seg1_ <- segregation::mutual_total(data=temp2b_,group="J4_1",unit="female"))
(seg1_ <- segregation::mutual_total(data=temp2c_,group="J4_1",unit="female"))
(seg1_ <- segregation::mutual_total(data=temp2d_,group="J4_1",unit="female"))
(seg1_ <- segregation::mutual_total(data=temp2e_,group="J4_1",unit="female"))


table(temp2a_$female)
table(temp2b_$female)
table(temp2c_$female)
table(temp2d_$female)
table(temp2e_$female)


## Maybe not?

df_mincer <- temp1  # now updated, but not saved yet.

table(df_mincer$occup)
# First, aggregating military men
df_mincer$occup <- ifelse(df_mincer$occup == 0, 110, df_mincer$occup) 

# For simplicity those with one digit go to the first category with 2 digits (e.g., 1 go to 11)
df_mincer$occup <- as.numeric(ifelse(df_mincer$occup<10,
                                     paste0(df_mincer$occup, "1"),
                                     df_mincer$occup))

# Leaving only 2 digits
df_mincer$occup2d <- as.numeric(substr(df_mincer$occup, 1, 2)) # leaving 2 digits

# If N obs is < 30 we technically cannot run a regression
# Let's aggregate such categories with the respective closest category
# I detected them manually for the easiness of computations base on this table:
table(df_mincer$occup2d, df_mincer$YEAR)

# Aggregating
df_mincer$occup2d[df_mincer$occup2d == 11|df_mincer$occup2d == 12] <- 112
df_mincer$occup2d[df_mincer$occup2d == 24|df_mincer$occup2d == 25] <- 245
df_mincer$occup2d[df_mincer$occup2d == 34|df_mincer$occup2d == 35] <- 345
df_mincer$occup2d[df_mincer$occup2d == 41|df_mincer$occup2d == 42] <- 412
df_mincer$occup2d[df_mincer$occup2d == 43|df_mincer$occup2d == 44] <- 434
df_mincer$occup2d[df_mincer$occup2d == 53|df_mincer$occup2d == 54] <- 534
df_mincer$occup2d[df_mincer$occup2d == 73|df_mincer$occup2d == 74|
                  df_mincer$occup2d == 75] <- 7345
df_mincer$occup2d[df_mincer$occup2d == 81|df_mincer$occup2d == 82] <- 812
df_mincer$occup2d[df_mincer$occup2d == 92|df_mincer$occup2d == 93|
                  df_mincer$occup2d == 94|df_mincer$occup2d == 95|
                  df_mincer$occup2d == 96] <- 923456
df_mincer$occup2d[df_mincer$occup2d == 61|df_mincer$occup2d == 62] <- 71

table(df_mincer$occup2d, df_mincer$YEAR)
# 345 category is too small even within its digit so 
# we need to merge it with another digit
df_mincer$occup2d[df_mincer$occup2d == 345] <- 412

# Checking
tbl <- as.data.frame(table(df_mincer$occup2d, df_mincer$YEAR))
tiny <- as.numeric(as.character(
  unique(tbl$Var1[tbl$Freq<30]))); tiny # no categaries with < 30 obs

# Creating a dummy set for occupations
dummy_set <- dummy.code(df_mincer$occup2d)
colnames(dummy_set) <- paste0("occup", colnames(dummy_set), sep = "")
df <- cbind(df_mincer, dummy_set)

# Probit regression: developing a female - non-female typology of occupations

# Empty list where the regression output will be written
probit <- vector("list", length(unique(df$YEAR)))
for (i in seq(length(probit))){
  probit[[i]] <- vector("list", length(unique(colnames(dummy_set))))
}
seq_year <- unique(df$YEAR)

# Looping over each year and occupation
# takes ~15 sec
for(i in seq(length(seq_year))){
  for(j in seq(length(colnames(dummy_set)))){
    probit[[i]][[j]] <- glm(as.formula(paste0(colnames(dummy_set)[j], "~", "female")),
                          family = binomial(link = "probit"),
                          data = df[df$YEAR == seq_year[i],])
  }
}

# Naming
names(probit) <- seq_year

# Computng summary
smry <-  lapply(probit, function(x) {lapply(x, summary)})

# A table with coefficients for the female variable
tbl_fem_ <- c()
for (y in seq_year){
  for (n in seq(length(colnames(dummy_set)))){
   tbl_fem_ <- rbind.data.frame(tbl_fem_, cbind.data.frame(
    "female" = round(smry[[paste0(y)]][[n]]$coefficients[2,1], 2),
    "p-value" = round(smry[[paste0(y)]][[n]]$coefficients[2,4], 3))
    )   
  }
}

YEAR <- rep(seq_year, each = ncol(dummy_set))
occup <- rep(colnames(dummy_set), length(seq_year))
tbl_fem <- cbind.data.frame(YEAR, occup, tbl_fem_) # final table

# Female-dominated occupations
fem_occup_vec <- as.character(unique(
  tbl_fem[tbl_fem$female > 0 &
            tbl_fem$'p-value' < 0.05, "occup"]))

# Non-female occupations
nonfem_occup_vec <- as.character(unique(
  tbl_fem[!(tbl_fem$female > 0 &
            tbl_fem$'p-value' < 0.05), "occup"]))

# Occupations which are in both categories depending on a wave
fem_occup_vec[fem_occup_vec %in% nonfem_occup_vec]

# Let us examine those cases
both <- tbl_fem[tbl_fem$occup %in% 
                  fem_occup_vec[fem_occup_vec %in% nonfem_occup_vec],]
# occup14 is insignificant in the majoriy of waves -> let's put it in nonfem_occup
# occup245 is significant and positive almost each time -> let's put it in fem_occup
# occup51 is mostly significant and positive -> let's put it in fem_occup
# occup91 is mostly significant and positive -> let's put it in fem_occup

# Defining a variable with nonfem_occup
df$occup2d <- as.character(df$occup2d)
df$fem_occup <- ifelse(df$occup2d %in% 
                            substr(fem_occup_vec[!fem_occup_vec=="occup14"],
                                   6, nchar(fem_occup_vec[!fem_occup_vec=="occup14"])), 1, 0)

# Looking at the distribution (looks logical)
table(df$fem_occup)
table(df$fem_occup, df$female)

# Filtering the missings left
df <- df %>%
  filter(!is.na(wage) & !is.na(edu_4) & wage > 0)

df$fem_occup <- factor(df$fem_occup, 
                       levels = c(1,0),
                       labels = c("Female Occupations",
                                  "Non-female Occupations"))

# Adjusting to prices in 2018
cpi <- rio::import("cpi.xlsx")[,c(1,4)]
df <- df %>%
  left_join(cpi, by = "YEAR")

df$wage_adjusted_to_2018 <- df$wage*df$norm
# wages in 2018 are alomst 3 times as high as wages in 2000:
aggregate(wage_adjusted_to_2018 ~ YEAR, df, mean)
aggregate(wage ~ YEAR, df, mean)
#################### Regressions with depreciation of education ###################

# for all 2018 data

# Empty list where the regression output will be written
lm_dep <- vector("list", length(unique(df$YEAR)))
seq_year <- unique(df$YEAR)

# Looping over each year
for(i in seq(length(seq_year))){
  lm_dep[[i]] <- lm(log(wage_adjusted_to_2018) ~ edu_4 +
                                exper + 
                                I(exper^2) + 
                                exper*edu_4 +
                                I(exper^2)*edu_4,
                           data = df[df$YEAR == seq_year[i],])
}
names(lm_dep) <- seq_year
smry_lm_dep <- lapply(lm_dep, summary)

###################################### Model prediction: 2018 
df_2018 <- as.data.frame(df[df$YEAR == 2018,])
pred_y_2018 <- exp(predict(lm_dep[['2018']], df_2018, interval="conf"))
df_2018 <- cbind(df_2018, pred_y_2018)

# Finding maximums for 2018 jointly
grid <- expand.grid(edu_4 = factor(1:3,
                                   labels = c("Higher",
                                               "Secondary",
                                               "Vocational"))) 

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2018[
      df_2018$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

xmax <- unique(df_2018[df_2018$fit %in% ymax, "exper"])


###
temp2_ <- haven::zap_labels(df_2018) 

class(temp2_$exper)

temp2_ <- temp2_ %>% group_by(female) %>% 
          summarise(meanexp=mean(exper))

# Plot
ggplot(temp2_, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  facet_wrap(facets="female")+
  geom_line(size = 0.6) +
  geom_vline(aes(xintercept = xmax[1])) +
  geom_vline(aes(xintercept = xmax[2])) +
  geom_vline(aes(xintercept = xmax[3])) +
  geom_point(shape = 4, aes(x = xmax[1], y = ymax[1]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[2], y = ymax[3]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[3], y = ymax[2]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2018.png", width = 7.5, height = 4,
            units = "in")

###################################### Model prediction: 2009 
df_2009 <- as.data.frame(df[df$YEAR == 2009,])
pred_y_2009 <- exp(predict(lm_dep[['2009']], df_2009, interval="conf"))
df_2009 <- cbind(df_2009, pred_y_2009)

# Finding maximums for 2018 jointly
grid <- expand.grid(edu_4 = factor(1:3,
                                   labels = c("Higher",
                                              "Secondary",
                                              "Vocational"))) 

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2009[
    df_2009$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

xmax <- unique(df_2009[df_2009$fit %in% ymax, "exper"])

# Plot
ggplot(df_2009, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(size = 0.6) +
  geom_vline(aes(xintercept = xmax[1])) +
  geom_vline(aes(xintercept = xmax[2])) +
  geom_vline(aes(xintercept = xmax[3])) +
  geom_point(shape = 4, aes(x = xmax[1], y = ymax[1]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[2], y = ymax[3]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[3], y = ymax[2]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2009.png", width = 7.5, height = 4,
       units = "in")

###################################### Model prediction: 2000 
df_2000 <- as.data.frame(df[df$YEAR == 2000,])
pred_y_2000 <- exp(predict(lm_dep[['2000']], df_2000, interval="conf"))
df_2000 <- cbind(df_2000, pred_y_2000)

# Finding maximums for 2018 jointly
grid <- expand.grid(edu_4 = factor(1:3,
                                   labels = c("Higher",
                                              "Secondary",
                                              "Vocational"))) 

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2000[
    df_2000$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

xmax <- unique(df_2000[df_2000$fit %in% ymax, "exper"])

# Plot
ggplot(df_2000, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(size = 0.6) +
  geom_vline(aes(xintercept = xmax[1])) +
  geom_vline(aes(xintercept = xmax[2])) +
  geom_vline(aes(xintercept = xmax[3])) +
  geom_point(shape = 4, aes(x = xmax[1], y = ymax[1]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[2], y = ymax[3]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  geom_point(shape = 4,aes(x = xmax[3], y = ymax[2]), size = 1,
             show.legend = F, stroke = 1.5, color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2000.png", width = 7.5, height = 4,
       units = "in")


################################## for occupations separately

# Empty list where the regression output will be written
lm_dep <- vector("list", length(unique(df$YEAR)))
seq_year <- unique(df$YEAR)

# Looping over each year
for(i in seq(length(seq_year))){
  lm_dep[[i]] <- lm(log(wage_adjusted_to_2018) ~ edu_4 +
                      exper + 
                      I(exper^2) + 
                      fem_occup +
                      fem_occup*exper +
                      fem_occup*I(exper^2) +
                      fem_occup*edu_4 +
                      exper*edu_4 +
                      I(exper^2)*edu_4 +
                      fem_occup*edu_4*exper +
                      fem_occup*edu_4*I(exper^2),
                    data = df[df$YEAR == seq_year[i],])
}
names(lm_dep) <- seq_year
smry_lm_dep <- lapply(lm_dep, summary)

###################################### Model prediction: 2018
df_2018 <- as.data.frame(df[df$YEAR == 2018,])
pred_y_2018 <- exp(predict(lm_dep[['2018']], df_2018, interval="conf"))
df_2018 <- cbind(df_2018, pred_y_2018)

# Finding maximums separately for occupational facets
grid <- expand.grid(fem_occup = 
                    factor(0:1, labels = c("Female Occupations",
                                           "Non-female Occupations")),
                    edu_4 = factor(1:3, labels = c("Higher",
                                                   "Secondary",
                                                   "Vocational"))) %>%
  arrange(fem_occup)
  
ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2018[
     df_2018$fem_occup == as.character(grid[i, "fem_occup"]) &
     df_2018$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

max <- cbind(grid, ymax)
xmax <- unique(df_2018[df_2018$fit %in% ymax, c("exper", "fit")])
names(xmax)[2] <- "ymax"

max_f <- max %>%
  left_join(xmax, by = "ymax")

# Plot
ggplot(df_2018, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                               linetype = edu_4)) +
   geom_line(aes(y = fit), size = 0.6) +
   geom_vline(data = filter(df_2018, fem_occup == "Female Occupations"),
               aes(xintercept = max_f$exper[1])) +
   geom_vline(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(xintercept = max_f$exper[2])) +
   geom_vline(data = filter(df_2018, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[3])) +
   geom_vline(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(xintercept = max_f$exper[4])) +
   geom_vline(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(xintercept = max_f$exper[5])) +
   geom_vline(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(xintercept = max_f$exper[6])) +
   geom_point(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(x = max_f$exper[1], y = max_f$ymax[1]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(x = max_f$exper[2], y = max_f$ymax[2]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Female Occupations"),
              aes(x = max_f$exper[3], y = max_f$ymax[3]), size = 1,
              show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(x = max_f$exper[4], y = max_f$ymax[4]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(x = max_f$exper[5], y = max_f$ymax[5]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   geom_point(data = filter(df_2018, fem_occup == "Non-female Occupations"),
              aes(x = max_f$exper[6], y = max_f$ymax[6]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
   facet_grid(~ as.factor(fem_occup)) +
   theme(legend.title = element_blank(),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.key = element_rect(size = 12))  +
   scale_color_manual(values = c("blue", "red", "darkgreen")) + 
   scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
   scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
   ylab("Monthly wage, RUB") +
   xlab("Experience") 

ggsave("2018_int.png", width = 7.5, height = 4,
             units = "in")

###################################### Model prediction: 2009
df_2009 <- as.data.frame(df[df$YEAR == 2009,])
pred_y_2009 <- exp(predict(lm_dep[['2009']], df_2009, interval="conf"))
df_2009 <- cbind(df_2009, pred_y_2009)

# Finding maximums separately for occupational facets
grid <- expand.grid(fem_occup = 
                      factor(0:1, labels = c("Female Occupations",
                                             "Non-female Occupations")),
                    edu_4 = factor(1:3, labels = c("Higher",
                                                   "Secondary",
                                                   "Vocational"))) %>%
  arrange(fem_occup)

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2009[
    df_2009$fem_occup == as.character(grid[i, "fem_occup"]) &
      df_2009$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

max <- cbind(grid, ymax)
xmax <- unique(df_2009[df_2009$fit %in% ymax, c("exper", "fit")])
names(xmax)[2] <- "ymax"

max_f <- max %>%
  left_join(xmax, by = "ymax")

# Plot
ggplot(df_2009, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(aes(y = fit), size = 0.6) +
  geom_vline(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[1])) +
  geom_vline(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[2])) +
  geom_vline(data = filter(df_2009, fem_occup == "Female Occupations"),
           aes(xintercept = max_f$exper[3])) +
  geom_vline(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[4])) +
  geom_vline(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[5])) +
  geom_vline(data = filter(df_2009, fem_occup == "Non-female Occupations"),
            aes(xintercept = max_f$exper[6])) +
  geom_point(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[1], y = max_f$ymax[1]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[2], y = max_f$ymax[2]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[3], y = max_f$ymax[3]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[4], y = max_f$ymax[4]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[5], y = max_f$ymax[5]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2009, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[6], y = max_f$ymax[6]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  facet_grid(~ as.factor(fem_occup)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2009_int.png", width = 7.5, height = 4,
       units = "in")

###################################### Model prediction: 2000
df_2000 <- as.data.frame(df[df$YEAR == 2000,])
pred_y_2000 <- exp(predict(lm_dep[['2000']], df_2000, interval="conf"))
df_2000 <- cbind(df_2000, pred_y_2000)

# Finding maximums separately for occupational facets
grid <- expand.grid(fem_occup = 
                      factor(0:1, labels = c("Female Occupations",
                                             "Non-female Occupations")),
                    edu_4 = factor(1:3, labels = c("Higher",
                                                   "Secondary",
                                                   "Vocational"))) %>%
  arrange(fem_occup)

ymax <- c()
for (i in seq(nrow(grid))){
  ymax <- c(ymax, max(df_2000[
    df_2000$fem_occup == as.character(grid[i, "fem_occup"]) &
      df_2000$edu_4 == as.character(grid[i, "edu_4"]), "fit"]))
}

max <- cbind(grid, ymax)
xmax <- unique(df_2000[df_2000$fit %in% ymax, c("exper", "fit")])
names(xmax)[2] <- "ymax"

max_f <- max %>%
  left_join(xmax, by = "ymax")

# Plot
ggplot(df_2000, aes(x = exper, y = fit, group = edu_4, color = edu_4,
                    linetype = edu_4)) +
  geom_line(aes(y = fit), size = 0.6) +
  geom_vline(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[1])) +
  geom_vline(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(xintercept = max_f$exper[2])) +
  geom_vline(data = filter(df_2000, fem_occup == "Female Occupations"),
           aes(xintercept = max_f$exper[3])) +
  geom_vline(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[4])) +
  geom_vline(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[5])) +
  geom_vline(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(xintercept = max_f$exper[6])) +
  geom_point(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[1], y = max_f$ymax[1]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Female Occupations"),
             aes(x = max_f$exper[2], y = max_f$ymax[2]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Female Occupations"),
            aes(x = max_f$exper[3], y = max_f$ymax[3]), size = 1,
          show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[4], y = max_f$ymax[4]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[5], y = max_f$ymax[5]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  geom_point(data = filter(df_2000, fem_occup == "Non-female Occupations"),
             aes(x = max_f$exper[6], y = max_f$ymax[6]), size = 1,
             show.legend = F, color = "black", shape = 4, stroke = 1.5) +
  facet_grid(~ as.factor(fem_occup)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(size = 12))  +
  scale_color_manual(values = c("blue", "red", "darkgreen")) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  scale_y_continuous(limits = c(2500, 35000), breaks = seq(2500, 35000, 5000)) +
  ylab("Monthly wage, RUB") +
  xlab("Experience") 

ggsave("2000_int.png", width = 7.5, height = 4,
       units = "in")

################# Model prediction (for each year)
#for (i in 1:length(seq_year)){
#  df_year <- as.data.frame(df[df$YEAR == seq_year[i],])
#  pred_y <- exp(predict(lm_dep[[i]], df_year, interval="conf"))
#  df_year <- cbind(df_year, pred_y)
  
# Plot
# p_int <- ggplot(df_year, aes(x = exper, y = fit, group = edu_4, color = edu_4,
#                  linetype = edu_4)) +
#    geom_line(aes(y = fit), size = 1.2) +
#    geom_ribbon(aes(ymin=lwr, ymax=upr, fill = edu_4), alpha = 0.1, colour = NA) +
#    facet_grid(~ as.factor(fem_occup)) +
#    theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         panel.grid.minor = element_blank(),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.key = element_rect(size = 12))  +
#   scale_color_manual(values = c("blue", "red", "darkgreen")) + 
#    scale_fill_manual(values=c("blue", "red", "darkgreen")) +
#    scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
#    scale_y_continuous(limits = c(0, 3)) +
#    ylab("Monthly wage normed by median") +
#    xlab("Experience") 
  
# ggsave(paste0("p_", seq_year[i], "_int.png"),  width = 7.5, height = 4,
#       units = "in")
# print(i)
#}

