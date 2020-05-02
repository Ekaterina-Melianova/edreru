# rosstat2a.R

library(dplyr)
library(tidyr)
library(lme4)
library(rio)

###### Rosstat main 2018 dataset
wd <- 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3'
setwd(wd)
Rosstat18 <- readRDS('Rosstat18.rds')
names(Rosstat18)[1] <- 'OKATO'

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
  select(region_name, mean_ege_score_2018_free_)  %>% 
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
RoR_15 <- RoR_1990_2015_rub %>% filter(year == 2015) %>% select (region, all_of(demand_vars)) 
RoR_15$demand_sum <- rowSums(RoR_15[,demand_vars])
RoR_15 <- RoR_15 %>% select (region, demand_sum, all_of(demand_vars))
names(RoR_15)[which(names(RoR_15) == 'region')] <- 'RoR_names'

# Merging with the main Rosstat data
Rosstat18 <- Rosstat18 %>% 
  left_join(RoR_15, by = 'RoR_names')

# Selecting only ranks
Rosstat18 <- Rosstat18 %>%
  select(OKATO, en_rgnames, univ_deg_share, ege_score, demand_sum, all_of(demand_vars))

# Remove duplicates
Ranks_demand_supply <- Rosstat18[!duplicated(Rosstat18$en_rgnames),]

# Reading RoREs
RoREs <- import('RoREs_cleaned.xlsx') %>% select(en_rgnames, re_HE_all_2018, re_VE_all_2018) 

# Merging with demand and supply side variables
Ranks <- RoREs %>%
  left_join(Ranks_demand_supply, by = 'en_rgnames')

# Filling missings in Nenetskiy Aok by values in Arkhangelskaya Oblast
# since the former is a part of the latter
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'demand_sum'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'demand_sum']
Ranks[Ranks$en_rgnames == 'Nenetskiy Aok', 'ege_score'] <- 
  Ranks[Ranks$en_rgnames == 'Arkhangelskaya Oblast', 'ege_score']

# NA removing
Ranks <- na.omit(Ranks)

# df for corplot
cormat_df <- Ranks

# adding ranks
Ranks$rank_univ <- rank(-Ranks$univ_deg_share)
Ranks$rank_ege <- rank(-Ranks$ege_score)
Ranks$rank_demand <- rank(-Ranks$demand_sum)
Ranks$rank_re_HE <- rank(-Ranks$re_HE_all_2018)
Ranks$rank_re_VE <- rank(-Ranks$re_VE_all_2018)

Ranks$rank_supply <- rowMeans(Ranks[, c('rank_univ', 'rank_ege')])

# Arranging
Ranks <- Ranks %>% select(en_rgnames, OKATO, rank_supply,
                        rank_demand, rank_re_HE, rank_re_VE)


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
Ranks$Ql_re_high_dem_greater <- ifelse(Ranks$rank_re_HE < 28 & 
                                                Ranks$rank_demand > Ranks$rank_supply, 1, 0)
Ranks$Ql_re_high_dem_lower <- ifelse(Ranks$rank_re_HE < 28 & 
                                              Ranks$rank_demand < Ranks$rank_supply, 1, 0)
Ranks$Ql_re_low_dem_greater <- ifelse(Ranks$rank_re_HE >= 28 & 
                                              Ranks$rank_demand > Ranks$rank_supply, 1, 0)
Ranks$Ql_re_low_dem_lower <- ifelse(Ranks$rank_re_HE >= 28 & 
                                               Ranks$rank_demand < Ranks$rank_supply, 1, 0)

export(Ranks, 'Ranks.xlsx')
# in Ranks_modified the regions are arranged manually according to our classification


############ Overall correlations matrix for IV

# demand_vars - Mirkina variables (demand side)
# EGE - EGE scores in a region
# univ_degree_share - proportion of people with university degree in the sample


library(PerformanceAnalytics)
cormat_df <- cormat_df %>% select(univ_deg_share, ege_score, all_of(demand_vars))
names(cormat_df) <- c('univ_share', 'EGE', 'agric_gdp', 'fishery_gdp',
                              'mining_gdp', 'manuf_gdp', 'sale_gdp', 'horeca_gdp', 'transp_gdp')

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
  if(missing(cex.cor)) cex <- 0.5/strwidth(txt)
  
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
      cex.labels = 1.8, font.labels = 2)

