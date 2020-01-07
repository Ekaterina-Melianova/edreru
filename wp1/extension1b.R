# extension1b.R
options(scipen=999) # to supress scientific notation
# WP1 extension along lines of Neuman-Weiss 1995.

# Revised by Suhas; Tuesday, January 07, 2020

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

############
# Adjusting to prices in 2018
cpi <- rio::import("cpi_revised.xlsx")[,c(1,6)]
df_mincer <- df_mincer %>% left_join(cpi,by="YEAR")
df_mincer2 <- df_mincer %>% 
  mutate(wage_c18=ifelse(YEAR >=1998,wage*cons_wb,(wage*cons_wb/1000)))

# Saving
saveRDS(df_mincer2, "df_mincer2.rds")
############

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
temp1 <- df_mincer2 %>%
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

FreqSP(temp1$J4_1)  # Function derived from descr/freq works only for cases with 9999996 etc sanitized
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

#############
#############
#############
#############
############# Adding indcat to the main 2018 df
temp1$J4_1 <- as.numeric(temp1$J4_1)
df_dep_18 <- temp1 %>% 
  filter (YEAR == 2018) %>%
  left_join(t2_[, c("J4_1", "indcat")], by = "J4_1")
#############
#############
#############
#############
#############


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
  
#ggsave("gen_ind18.png", width = 7.5, height = 4,
#       units = "in")



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

#############
#############
#############
#############
############# 
# Adding categorization by double digit occupational digit to the main 2018 df

# ! The same procedure as above but for the whole df
temp218_ <- df_dep_18 %>% mutate(isco_2=substr(occup,1,2))

t2 <- temp218_ %>% 
  mutate(isco_2=replace(isco_2,isco_2%in%c(0,35,61,62,95),66)) %>% arrange(isco_2) %>%   
  group_by(isco_2) %>% 
  summarize(tfem=sum(female),tall=n()) %>% arrange(desc(tall)) %>%
  mutate(pfem=tfem/tall) %>%  mutate(pfem=round(pfem,4)*100,pmal=100-pfem) %>% arrange(desc(pfem))

t2$ocpcat18 <- NA
t2$ocpcat18[t2$pfem<44] <- "occmale"
t2$ocpcat18[t2$pfem>=44&t2$pfem<=64] <- "occneut"
t2$ocpcat18[t2$pfem>64] <- "occfemale"

df_dep_18 <- df_dep_18 %>% mutate(isco_2 = substr(occup,1,2))
df_dep_18 <- df_dep_18 %>%
  left_join(t2[, c("isco_2", "ocpcat18")], by = "isco_2")

# Saving
saveRDS(df_dep_18, "df_dep_18_.rds")

#############
#############
#############
#############
#############

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


## temp218 - Basic earnings profile 

## let's take university graduates

temp218u_ <- temp218_ %>% filter(edu_4=="Higher")

ggplot(temp218_,aes(x=exper,y=wage,group=edu_4,col=edu_4)) + geom_smooth(se=FALSE, method=loess)+
   facet_wrap(~female)



##########################################################################################
# wages in 2018 are alomst 3 times as high as in 2000:
aggregate(wage_c18 ~ YEAR, df_mincer, mean)
aggregate(wage ~ YEAR, df_mincer, mean)


temp_ <- df_mincer %>% filter(edu_4=="Higher") %>% filter(YEAR==1998|YEAR==2006|YEAR==2018)
ggplot(temp_,aes(x=exper,y=wage_c18,group=as.factor(YEAR),linetype=as.factor(YEAR))) + geom_smooth(se=FALSE,col="red",lwd=0.75,method=loess)+
  coord_cartesian(ylim=c(0,40000))+
  xlab("Experience in years") + ylab("Monthly wages in 2018 Rubles")+
  scale_linetype_manual(values=c("dotted","longdash","solid"))+
  theme(panel.background = element_rect(fill = "#edfca1")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = "none")

ggsave("dp01_he.png", width = 3, height = 3.5,
       units = "in")
  

temp_ <- df_mincer %>% filter(edu_4=="Vocational") %>% filter(YEAR==1998|YEAR==2006|YEAR==2018)
ggplot(temp_,aes(x=exper,y=wage_c18,group=as.factor(YEAR),linetype=as.factor(YEAR))) + geom_smooth(se=FALSE,col="blue",lwd=0.75,method=loess)+
  coord_cartesian(ylim=c(0,40000))+
  scale_linetype_manual(values=c("dotted","longdash","solid"))+
  xlab("Experience in years") + ylab("Monthly wages in 2018 Rubles")+
  theme(panel.background = element_rect(fill = "cornsilk")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = "none")

ggsave("dp01_ve.png", width = 3, height = 3.5,
       units = "in")


temp_ <- df_mincer %>% filter(edu_4=="Secondary") %>% filter(YEAR==1998|YEAR==2006|YEAR==2018)
ggplot(temp_,aes(x=exper,y=wage_c18,group=as.factor(YEAR),linetype=as.factor(YEAR))) + geom_smooth(se=FALSE,col="darkgreen",lwd=0.75,method=loess)+
  coord_cartesian(ylim=c(0,40000))+
  scale_linetype_manual(values=c("dotted","longdash","solid"),
                        breaks = rev(levels(as.factor(temp_$YEAR))))+
  xlab("Experience in years") + ylab("Monthly wages in 2018 Rubles")+
  theme(panel.background = element_rect(fill = "#dbebf9")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = "none")
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "yellow"),
        legend.key = element_rect(fill = "yellow"),
        legend.title=element_blank())

ggsave("dp01_se.png", width = 3, height = 3.5,
       units = "in")


## Now the same plot, arranged with years together

temp_ <- df_mincer %>% filter(YEAR==1998) 
ggplot(temp_,aes(x=exper,y=wage_c18,group=as.factor(edu_4),color=as.factor(edu_4))) +
  geom_smooth(se=FALSE,lwd=0.75,method=loess)+
  coord_cartesian(ylim=c(0,40000))+
  scale_color_manual(values=c("green","blue","red"),
                        breaks = rev(levels(as.factor(temp_$edu_4))))+
  xlab("Experience in years") + ylab("Monthly wages in 2018 Rubles")+
  theme(panel.background = element_rect(fill = "#eeecec")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = c(0.3, 0.8),
        legend.background = element_rect(fill = "#e5ec7b"),
        legend.key = element_rect(fill = "#e5ec7b"),
        legend.title=element_blank())

ggsave("dp01_98.png", width = 3, height = 3.5,
       units = "in")

###

temp_ <- df_mincer %>% filter(YEAR==2006) 
ggplot(temp_,aes(x=exper,y=wage_c18,group=as.factor(edu_4),color=as.factor(edu_4))) +
  geom_smooth(se=FALSE,lwd=0.75,method=loess)+
  coord_cartesian(ylim=c(10000,40000))+
  scale_color_manual(values=c("green","blue","red"),
                     breaks = rev(levels(as.factor(temp_$edu_4))))+
  xlab("Experience in years") + ylab("Monthly wages in 2018 Rubles")+
  theme(panel.background = element_rect(fill = "#eeecec")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = "none")

ggsave("dp01_06.png", width = 3, height = 3.5,
       units = "in")


###

temp_ <- df_mincer %>% filter(YEAR==2018) 
ggplot(temp_,aes(x=exper,y=wage_c18,group=as.factor(edu_4),color=as.factor(edu_4))) +
  geom_smooth(se=FALSE,lwd=0.75,method=loess)+
  coord_cartesian(ylim=c(10000,40000))+
  scale_color_manual(values=c("green","blue","red"),
                     breaks = rev(levels(as.factor(temp_$edu_4))))+
  xlab("Experience in years") + ylab("Monthly wages in 2018 Rubles")+
  theme(panel.background = element_rect(fill = "#eeecec")) +
  theme(panel.grid.major = element_line(color="white")) +
  theme(panel.grid.major = element_line(size=1)) +
  theme(panel.grid.minor = element_line(color="white")) +
  theme(panel.grid.minor = element_line(size=1))+
  theme(legend.position = "none")

ggsave("dp01_18.png", width = 3, height = 3.5,
       units = "in")

## Just experimenting below
## Run Murillo Regression

lm_dep <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2),
             data=subset(df_mincer,YEAR==2018))
summary(lm_dep)
#b1 edu_yrs        0.05302502  0.00913908   5.802 0.000000006881165398 ***
#b2 edu_yrs:exper  0.00006204  0.00038075   0.163              0.87056  
#b3 exper          0.02265965  0.00748268   3.028              0.00247 ** 
#b4 I(exper^2)    -0.00061336  0.00007516  -8.161 0.000000000000000402 ***
  
(junk <- df_mincer %>% select(YEAR,edu_yrs,exper) %>% group_by(YEAR) %>% 
  summarize(meaned=mean(edu_yrs),meanex=mean(exper)))

#tdesc=0.00006204*13*100 at average s of 13 or 0.080652 i.e. 0.08%

#tdexp=2*0.00061336*22*100 at average of 22 ex or 2.698784 i.e. 2.698% 

lm_dep <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2),
             data=subset(df_mincer,YEAR==2018 & female==1))
summary(lm_dep)

# b1=0.05302502
# b2=0.00006204
# b3=0.02265965
# b4=-0.00061336

#b1  edu_yrs        0.06648222  0.01243787   5.345         0.0000000965 ***
#b2  edu_yrs:exper  0.00044313  0.00051476   0.861               0.3894    
#b3  exper          0.01991126  0.01001705   1.988               0.0469 *  
#b4  I(exper^2)    -0.00057140  0.00009656  -5.917         0.0000000036 ***

# b2_s + b3 + 2b4x
lm_dep <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2),
             data=subset(df_mincer,YEAR==2018 & female==0))
summary(lm_dep)

#b1  edu_yrs        0.0770274  0.0123341   6.245     0.00000000048807 ***
#b2  edu_yrs:exper -0.0007765  0.0005188  -1.497             0.134619  
#b3  exper          0.0371668  0.0102348   3.631             0.000287 ***
#b4  I(exper^2)    -0.0007403  0.0001058  -6.997     0.00000000000327 ***
  
## others 
lm_dep <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2),
             data=subset(df_mincer,YEAR==2018 & edu_4=="Higher"))
summary(lm_dep)


lm_dep <- lm(log(wage_c18) ~ edu_yrs + exper*edu_yrs + exper + I(exper^2),
             data=subset(df_mincer,YEAR==2018 & exper>5 & exper <40))
summary(lm_dep)




