# rosstat2b.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(rio)
library(ggrepel)
library(ggpmisc)

###### Rosstat main 2018 dataset
wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3'
setwd(wd)
Rosstat18 <- readRDS('Rosstat18.rds')
names(Rosstat18)[1] <- 'OKATO'

# Agerages for edu and lnwage for corplot
Rosstat18$lnwage <- log(Rosstat18$wage)

Rosstat18 <- Rosstat18 %>%
  group_by(OKATO) %>%
  mutate(edu_yrs_region = mean(edu_yrs),
         lnwage_region = mean(lnwage))

#### QUANTITY
# % workforce 25-65 with univ. degree 
Rosstat18 <- Rosstat18 %>%
  group_by(OKATO) %>%
  mutate(univ_deg_share = sum(edu_4 == "Higher")/n())

#### QUALITY
# EGE scores (Artem's database)
EGE_full <- import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/dataframe_universities.xlsx')
# if 2018 data is missing let us replace it with 2017 data where possible
EGE_full$mean_ege_score_2018_free_ <- ifelse(is.na(EGE_full$mean_ege_score_2018_free),
                                             EGE_full$mean_ege_score_2017_free, 
                                             EGE_full$mean_ege_score_2018_free)
# Ranking by ege scores   
EGE <- EGE_full %>% 
  dplyr::select(region_name, mean_ege_score_2018_free_)  %>% 
  group_by(region_name) %>%
  summarise(ege_score = mean(mean_ege_score_2018_free_, na.rm = T)) 

#export(EGE$region_name, 'region_name.xlsx')
region_name <- import('region_name.xlsx')

# Joining OKATO to the EGE df
EGE <- EGE %>% 
  left_join(region_name, by = 'region_name')

# Joining Rosstat by OKATO with the EGE df
Rosstat18 <- Rosstat18 %>% 
  left_join(EGE, by = 'OKATO')

# Mirkina df
RoR_1990_2015_rub <- readstata13::read.dta13('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/RoR_1990_2015_rub.dta')
demand_vars <- paste0('chapter_', letters[c(1:4, 7:9)])

# Taking last year
RoR_15 <- RoR_1990_2015_rub %>% filter(year == 2015) %>% 
  dplyr::select (region, all_of(demand_vars)) 
RoR_15$demand_sum <- rowSums(RoR_15[,demand_vars])
RoR_15 <- RoR_15 %>% 
  dplyr::select (region, demand_sum, all_of(demand_vars))
names(RoR_15)[which(names(RoR_15) == 'region')] <- 'RoR_names'

# Merging with the main Rosstat data
Rosstat18 <- Rosstat18 %>% 
  left_join(RoR_15, by = 'RoR_names')

# Selecting only ranks
Rosstat18 <- Rosstat18 %>%
  dplyr::select(OKATO, en_rgnames, univ_deg_share, ege_score,
         demand_sum, all_of(demand_vars), edu_yrs_region, lnwage_region)

# Remove duplicates
Ranks_demand_supply <- Rosstat18[!duplicated(Rosstat18$en_rgnames),]

# standardize labor supply variables
ege_zscore <- as.vector(round(psych::rescale(Ranks_demand_supply$ege_score,
                                                       mean=500, sd=100, df=FALSE)))

Ranks_demand_supply$ege_zscore <- ege_zscore

ege_uzscore <- as.vector(psych::rescale(Ranks_demand_supply$ege_score,
                                             mean=0, sd=1, df=FALSE))

Ranks_demand_supply$ege_uzscore <- ege_uzscore


univ_zcov <- as.vector(psych::rescale(Ranks_demand_supply$univ_deg_share,
                                            mean=0, sd=1, df=FALSE))
Ranks_demand_supply$univ_zcov <- univ_zcov



# Reading RoREs
RoREs <- import('RoREs_cleaned.xlsx') %>% 
  dplyr::select(en_rgnames, re_HE_all_2018, re_VE_all_2018) 

# Merging with demand and supply side variables
Ranks <- RoREs %>%
  left_join(Ranks_demand_supply, by = 'en_rgnames')

# Filling missings in Nenetskiy Aok by values in Arkhangelskaya Oblast
# since the former is a part of the latter
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'univ_zcov'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'univ_zcov']
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'ege_zscore'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'ege_zscore']
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'ege_uzscore'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'ege_uzscore']


Ranks <- Ranks[-c(86),] # deleting entry for Russian Federation
# NA removing
Ranks <- Ranks %>% filter(!is.na(ege_zscore) & !is.na(univ_zcov))

# adding ranks
#Ranks$rank_univ <- rank(-Ranks$univ_deg_share)
#Ranks$rank_ege <- rank(-Ranks$ege_score)

# Depressed regions
depressed_regions <- c('Respublika Adygeya', 'Pskovskaya Oblast',
                       'Altayskiy Kray', 'Kurganskaya Oblast',
                       'Respublika Kalmykiya', 'Chuvashskaya Respublika', 
                       'Respublika Altay', 'Respublika Karelia',
                       'Respublika Tyva', 'Respublika Mariy El')

 
Ranks <- Ranks %>% mutate(Dep_reg=ifelse(en_rgnames %in% depressed_regions,1,0))

# get eci data from BOFIT Lyubimov 2018 

eci18 <- read.csv("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3/eci18.csv")
names(eci18)[1] <- "bofit_name"
eci18 <- eci18 %>% transmute(bofit_name=bofit_name,
                             ECI=ECI,en_rgnames=en_rgnames,
                             OKATO=as.character(sprintf("%02d",OKATO))) %>%
                 dplyr::select(en_rgnames, ECI)


Ranks <- left_join(Ranks,eci18, by="en_rgnames")

eci18_z <- as.vector(psych::rescale(Ranks$ECI,
                                      mean=0, sd=1, df=FALSE))
Ranks$eci18_z <- eci18_z

demand_z <- as.vector(psych::rescale(Ranks$demand_sum,
                                    mean=0, sd=1, df=FALSE))
Ranks$demand_z <- demand_z



temp <- Ranks %>% filter(ege_zscore > 500 & univ_zcov > 2)
# Moscow and St. Pete's , of course
temp <- Ranks %>% filter(ege_zscore <= 300 | univ_zcov >= 2)

range(Ranks$demand_sum,na.rm = TRUE)
Ranks %>% filter(demand_sum < 35) # Tuva
Ranks %>% filter(demand_sum > 80) # Khanty-Mansisk


#Ranks$rank_eci <- rank(-Ranks$ECI)

# I make a scatter plot of ranks by quantity and quality of supply

ggplot(data=Ranks, aes(x=univ_zcov,y=ege_zscore, color=as.factor(Dep_reg)))+
  geom_point() +
  geom_text_repel(data=Ranks[Ranks$Dep_reg==1,], 
                  aes(label=en_rgnames,color="blue",segment.size  = 0.2,
                      hjust=1))+
  stat_quadrant_counts(xintercept = 0,yintercept = 500,colour = "darkgreen")+
  geom_hline(yintercept=500)+
  geom_vline(xintercept=0)+
  xlab("University educated share")+
  ylab("EGE scores")+
  scale_color_manual(values=c("black", "red","blue"))+
  scale_x_continuous(limits = c(-2,2))+
  scale_y_continuous(limits = c(300,700))+
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14))

ggsave("ranks1a.png", width = 4, height = 4,
       units = "in")



#Ranks$rank_demand <- rank(-Ranks$demand_sum)
#Ranks$rank_re_HE <- rank(-Ranks$re_HE_all_2018)
#Ranks$rank_re_VE <- rank(-Ranks$re_VE_all_2018)

#Ranks$rank_supply <- rowMeans(Ranks[, c('rank_univ', 'rank_ege')])

# Arranging
#Ranks <- Ranks %>% select(en_rgnames, OKATO, rank_supply,
#                        rank_demand, rank_re_HE, rank_re_VE)



# Filling missings in Nenetskiy Aok by values in Arkhangelskaya Oblast
# since the former is a part of the latter
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'demand_z'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'demand_z']
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'eci18_z'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'eci18_z']
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'eci18_z'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'eci18_z']

# I make a scatter plot of ranks by quantity and quality of demand

ggplot(data=Ranks, aes(x=demand_z,y=eci18_z, color=as.factor(Dep_reg)))+
  geom_point() +
  geom_text_repel(data=Ranks[Ranks$Dep_reg==1,], 
                  aes(label=en_rgnames,color="blue",segment.size  = 0.2,
                      hjust=1))+
  stat_quadrant_counts(xintercept = 0,yintercept = 0,colour = "darkgreen")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  xlab("Demand quantity from industry shares")+
  ylab("Demand quality from ECI")+
  scale_color_manual(values=c("black", "red","blue"))+
#  scale_x_continuous(limits = c(-2,2))+
 scale_y_continuous(limits = c(-2,2))+
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14))

ggsave("ranks1b.png", width = 4, height = 4,
       units = "in")

# Tag quadrant membership by Supply Quadrant and Demand Quadrant
Ranks$SQ <- 0
Ranks$SQ[Ranks$univ_zcov>=0 & Ranks$ege_zscore>=500] <- "I"
Ranks$SQ[Ranks$univ_zcov<0 & Ranks$ege_zscore>=500] <- "II"
Ranks$SQ[Ranks$univ_zcov<0 & Ranks$ege_zscore<500] <- "III"
Ranks$SQ[Ranks$univ_zcov>=0 & Ranks$ege_zscore<500] <- "IV"
table(Ranks$SQ)

Ranks$DQ <- 0
Ranks$DQ[Ranks$demand_z>=0 & Ranks$eci18_z>=0] <- "I"
Ranks$DQ[Ranks$demand_z<0 & Ranks$eci18_z>=0] <- "II"
Ranks$DQ[Ranks$demand_z<0 & Ranks$eci18_z<0] <- "III"
Ranks$DQ[Ranks$demand_z>=0 & Ranks$eci18_z<0] <- "IV"
table(Ranks$DQ)


###############################################################################################
# Depressed regions
#depressed_regions <- c('Respublika Adygeya', 'Pskovskaya Oblast',
#                       'Altayskiy Kray', 'Kurganskaya Oblast',
#                       'Respublika Kalmykiya', 'Chuvashskaya Respublika', 
#                       'Respublika Altay', 'Respublika Karelia',
#                       'Respublika Tyva', 'Respublika Mariy El')

# Filtering depressed regions
#Ranks_depressed <- filter(Ranks, en_rgnames %in% depressed_regions)

# Groups 

Ranks$DQ <- factor(Ranks$DQ,levels=c("I","II","IV","III")) 
Ranks$SQ <- factor(Ranks$SQ,levels=c("I","II","IV","III")) 

library(gmodels)
gmodels::CrossTable(Ranks$DQ,Ranks$SQ)



# I now get demand dominant or supply dominant classification

Ranks$DEMSUP <- 0
Ranks$DEMSUP[Ranks$DQ=="I" & Ranks$SQ !="I"] <- "DEMDOM" 
Ranks$DEMSUP[Ranks$SQ=="I" & Ranks$DQ !="I"] <- "SUPDOM" 

Ranks$DEMSUP[Ranks$DQ== "II"  & Ranks$SQ== "III"] <- "DEMDOM" 
Ranks$DEMSUP[Ranks$DQ== "IV" & Ranks$SQ== "III"] <- "DEMDOM" 

Ranks$DEMSUP[Ranks$SQ== "II"  & Ranks$DQ== "III"] <- "SUPDOM" 
Ranks$DEMSUP[Ranks$SQ== "IV" & Ranks$DQ== "III"] <- "SUPDOM" 

table(Ranks$DEMSUP)

Ranks$TAG_ <- 0
Ranks$TAG_[Ranks$DEMSUP==0] <- 1

# Now get dem or sup for rest 28 cases


Ranks$DEMSUP[Ranks$TAG_==1 & Ranks$ege_uzscore > Ranks$eci18_z] <- "SUPDOM"
Ranks$DEMSUP[Ranks$TAG_==1 & Ranks$ege_uzscore < Ranks$eci18_z] <- "DEMDOM"


table(Ranks$DEMSUP)


# Returns

median(Ranks$re_HE_all_2018)
# 92.54144

median(Ranks$re_VE_all_2018)
# 31.26893 

Ranks$RETURNS <- 0

Ranks$RETURNS[Ranks$re_HE_all_2018 >= 92.54144 & 
                Ranks$re_VE_all_2018>=31.26893] <- "High"

Ranks$RETURNS[Ranks$re_HE_all_2018 <  92.54144 & 
                Ranks$re_VE_all_2018 < 31.26893] <- "Low"

Ranks$TAG_ <- 0
Ranks$TAG_[Ranks$RETURNS==0] <- 1
table(Ranks$TAG_)

Ranks$RETURNS[Ranks$TAG_==1 & Ranks$re_HE_all_2018 >= 92.54144] <- "High"
Ranks$RETURNS[Ranks$TAG_==1 & Ranks$re_HE_all_2018 < 92.54144] <- "Low"

table(Ranks$RETURNS)

# Now for the all important cross table

CrossTable(Ranks$DEMSUP,Ranks$RETURNS)


Ranks2 <- Ranks %>% dplyr::select(en_rgnames,RETURNS,DEMSUP,Dep_reg)


Ranks2$facvar <- 0
Ranks2$facvar[Ranks2$RETURNS=="High" & Ranks2$DEMSUP=="DEMDOM"] <- "High Returns - Demand Dominates"
Ranks2$facvar[Ranks2$RETURNS=="Low" & Ranks2$DEMSUP=="DEMDOM"] <- "Low Returns - Demand Dominates"
Ranks2$facvar[Ranks2$RETURNS=="High" & Ranks2$DEMSUP=="SUPDOM"] <- "High Returns - Supply Dominates"
Ranks2$facvar[Ranks2$RETURNS=="Low" & Ranks2$DEMSUP=="SUPDOM"] <- "Low Returns - Supply Dominates"

Ranks2$facvar <- factor(Ranks2$facvar,levels=c("High Returns - Demand Dominates",
                                               "Low Returns - Demand Dominates",
                                               "High Returns - Supply Dominates",
                                               "Low Returns - Supply Dominates")) 

Ranks2$xx <- 0
set.seed(2)
Ranks2$xx <- sample(1:500, 20, replace=F)
Ranks2$yy <- sample(1:500, 20, replace=F)

Ranks2$yy[Ranks2$en_rgnames=="Nizhegorodskaya Oblast"] <- 300


ggplot(data=Ranks2,aes(x=xx,y=yy,
         label=en_rgnames,color=as.factor(Dep_reg)))+ 
  geom_text_repel(segment.color='transparent',force=2.5,vjust=0.75,
                  direction="both",size=4)+
  scale_color_manual(values=c("black","red"))+
  facet_wrap(~facvar)+
  xlab("Returns to Education")+
  ylab("Demand or Supply Dominates")+
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color = "blue", size = 14, face="bold"),
        axis.title.y = element_text(color = "blue", size = 14, face="bold"),
        strip.text = element_text(face="bold", size=14),
        strip.background=element_rect(fill="lightblue", colour="black"))

ggsave("ranks2a.png", width = 10, height = 10,
       units = "in")



export(Ranks, 'Ranks.xlsx')
# in Ranks_modified the regions are arranged manually according to our classification


############ Overall correlations matrix for IV

# demand_vars - Mirkina variables (demand side)
# EGE - EGE scores in a region
# univ_degree_share - proportion of people with university degree in the sample


library(PerformanceAnalytics)

# df for corplot
cormat_df <- Ranks
cormat_df <- cormat_df %>% select(en_rgnames, univ_deg_share, ege_score, all_of(demand_vars))
cormat_df <- cormat_df %>%
  left_join(Rosstat18[, c('en_rgnames', 'edu_yrs_region', 'lnwage_region')], by = "en_rgnames")
cormat_df <- cormat_df[!duplicated(cormat_df$en_rgnames),]
cormat_df[1] <- NULL
names(cormat_df) <- c('univ_share', 'EGE', 'agric_gdp', 'fishery_gdp',
                      'mining_gdp', 'manuf_gdp', 'sale_gdp',
                      'horeca_gdp', 'transp_gdp',
                      "edu_yrs", "lnwage" )
cormat_df <- cormat_df[,c("edu_yrs", "lnwage", 'univ_share', 'EGE',
                         'agric_gdp', 'fishery_gdp',
                         'mining_gdp', 'manuf_gdp', 'sale_gdp',
                         'horeca_gdp', 'transp_gdp')]
# Elements of the plot
hist.panel = function (x, ...) {
  par(new = TRUE)
  hist(x,
       col = "light gray",
       probability = T,
       axes = FALSE,
       main = "",
       breaks = "FD")
}
panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs",
                      method = 'pearson', cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use=use, method=method) # MG: remove abs here
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 2
  
  test <- cor.test(x,y, method=method)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  # MG: add abs here and also include a 30% buffer for small numbers
  text(0.5, 0.5, txt, cex = cex)
  text(.8, .8, Signif, cex=cex, col=2)
}

# Plotting cor matrix
pairs(cormat_df, gap=0, lower.panel=panel.smooth,
      upper.panel=panel.cor, diag.panel=hist.panel,
      cex.labels = 1.5, font.labels = 2)

