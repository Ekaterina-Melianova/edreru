# rosstat1a.R

library(foreign)
library(plyr); library(dplyr)
library(gmodels)
library(lmtest)
library(sqldf)
library("XLConnectJars",lib.loc="C:/Users/wb164718/Documents/R/win-library/3.5")
library(questionr)
library(labelled)
library(tidyr)
library(magrittr)
library(ggplot2)
library(data.table)
library(pbapply)
library(gridExtra)
library(stargazer)
library(xtable)

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
df <- df_[df_$H01_02 >= 25 & df_$H01_02 < 65,]

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

###########################################################################################################
########################################### Regression ####################################################
###########################################################################################################

# Empty list where the regression output will be written
# The list is a list of 5 lists - one for each year of data
# 2014, 2015, 2016, 2017, 2018
# and for each year about 85 being the number of regions (H00_02)

Rlm_mincer_all <- vector("list", length(unique(df$YEAR)))
for (i in seq(length(Rlm_mincer_all))){
  Rlm_mincer_all[[i]] <- vector("list", length(unique(df$H00_02)))
}
Rlm_mincer_f = Rlm_mincer_m = Rlm_mincer_all 


# define indices to simplify various loops 
seq_year <- unique(df$YEAR)
df$H00_02 <- as.numeric(as.character(df$H00_02))
seq_region <- unique(df$H00_02) %>% sort()

# Looping over each year and region
# First loop is over year - and then regions within year 
# and skip for regions 35 and 67 in year 2014
# Then the algorithm starts filling in each i,j list
# We are running 85*5 = 425-2 = 423 regressions
# data we have to choose rows - YEAR from i & region from j, weights 
# is the column KVZV ; print(i) is just to display the loop is working

# All
# takes ~ 10 sec
for(i in seq(length(seq_year))){
  for(j in seq(length(seq_region))){
    # Accounting for the absence of data in 2014 for Crimea and Sevastopol
    if(!((j == which(seq_region == 35)| 
          j == which(seq_region == 67)) 
         & i == 1)){
      Rlm_mincer_all[[i]][[j]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + female,
                                   data = df[(df$YEAR == seq_year[i] &
                                            df$H00_02 == seq_region[j]),],
                                   weights = df[(df$YEAR == seq_year[i] &
                                                 df$H00_02 == seq_region[j]), "KVZV"])    
    }
  }
  print(i)
}

# By gender
for(i in seq(length(seq_year))){
  for(j in seq(length(seq_region))){
    # Accounting for the absence of data in 2014 for Crimea and Sevastopol
    if(!((j == which(seq_region == 35)|
          j == which(seq_region == 67)) 
         & i == 1)){
      Rlm_mincer_f[[i]][[j]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                                   data = df[(df$YEAR == seq_year[i] &
                                              df$H00_02 == seq_region[j]) & 
                                              df$female == 1,],
                                   weights = df[(df$YEAR == seq_year[i] &
                                                 df$H00_02 == seq_region[j]) & 
                                                 df$female == 1, "KVZV"])   
      Rlm_mincer_m[[i]][[j]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                                   data = df[(df$YEAR == seq_year[i] &
                                              df$H00_02 == seq_region[j]) & 
                                              df$female == 0,],
                                   weights = df[(df$YEAR == seq_year[i] &
                                                 df$H00_02 == seq_region[j]) & 
                                                 df$female == 0, "KVZV"])
    }
  }
  print(i)
}

# Naming the list for each year

names(Rlm_mincer_all) <- seq_year
names(Rlm_mincer_m) <- seq_year
names(Rlm_mincer_f) <- seq_year


# We now run the regression for the whole country, without reference to region
# five sets of regressions
##################################### FOR THE WHOLE SAMPLE ###############################
# Empty list where the regression output will be written
lm_mincer_all <- vector("list", length(unique(df$YEAR)))
for (i in seq(length(lm_mincer_all))){
  lm_mincer_all[[i]] <- vector("list", length(unique(df$H00_02)))
}
lm_mincer_f = lm_mincer_m  = lm_mincer_all 
seq_year <- unique(df$YEAR)



# Looping over each year
for(i in seq(length(seq_year))){
  lm_mincer_all[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2) + female,
                           data = df[df$YEAR == seq_year[i],],
                           weights = df[df$YEAR == seq_year[i], "KVZV"])
                    
  lm_mincer_f[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                         data = df[df$YEAR == seq_year[i] & df$female == 1,],
                         weights = df[df$YEAR == seq_year[i]& df$female == 1, "KVZV"])
  
  lm_mincer_m[[i]] <- lm(log(wage) ~ edu_4 + exper + I(exper^2),
                         data = df[df$YEAR == seq_year[i] &  df$female == 0,],
                         weights = df[df$YEAR == seq_year[i]& df$female == 0, "KVZV"]) 
  
  print(i)
}
names(lm_mincer_all) <- seq_year
names(lm_mincer_f) <- seq_year
names(lm_mincer_m) <- seq_year

# Joining total results with the regression results by regions


for (i in 1:length(Rlm_mincer_all)){
  Rlm_mincer_all[[i]][[length(Rlm_mincer_all[[i]]) + 1]] <- lm_mincer_all[[i]]
  Rlm_mincer_f[[i]][[length(Rlm_mincer_f[[i]]) + 1]] <- lm_mincer_f[[i]]
  Rlm_mincer_m[[i]][[length(Rlm_mincer_m[[i]]) + 1]] <- lm_mincer_m[[i]]
}
############################################################################################

# A file with region names
Sys.setlocale("LC_CTYPE", "russian")
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3"
setwd(wd)
rgvars <- rio::import("rgvars.xlsx") %>% arrange(OKATO)

# Note the strict correspondence necessary between OKATO and H00_02

# Naming sublists with regression summary
for (i in seq(length(seq_year))){
  names(Rlm_mincer_all[[i]]) <- c(rgvars[,3], "Russian Federation")
  names(Rlm_mincer_m[[i]]) <- c(rgvars[,3], "Russian Federation")
  names(Rlm_mincer_f[[i]]) <- c(rgvars[,3], "Russian Federation")
}

# Summarizing
Rsmry_all <- lapply(Rlm_mincer_all, function(x) {lapply(x, summary)})
Rsmry_f <- lapply(Rlm_mincer_m, function(x) {lapply(x, summary)})
Rsmry_m <- lapply(Rlm_mincer_f, function(x) {lapply(x, summary)})

# Calculating returns by year for higher and vocational education

# Setting variable names
# Higher - all
re_h <- paste0("re_HE_all_", seq_year)
p_h <- paste0("p_HE_all_", seq_year)
ci_up_h <- paste0("ci_up_HE_all_", seq_year)
ci_lo_h <- paste0("ci_lo_HE_all_", seq_year)

# Higher - males
re_h_m <- paste0("re_HE_m_", seq_year)
p_h_m <- paste0("p_HE_m_", seq_year)
ci_up_h_m <- paste0("ci_up_HE_m_", seq_year)
ci_lo_h_m <- paste0("ci_lo_HE_m_", seq_year)

# Higher - females
re_h_f <- paste0("re_HE_f_", seq_year)
p_h_f <- paste0("p_HE_f_", seq_year)
ci_up_h_f <- paste0("ci_up_HE_f_", seq_year)
ci_lo_h_f <- paste0("ci_lo_HE_f_", seq_year)

# Vocational - all
re_v <- paste0("re_VE_all_", seq_year)
p_v <- paste0("p_VE_all_", seq_year)
ci_up_v <- paste0("ci_up_VE_all_", seq_year)
ci_lo_v <- paste0("ci_lo_VE_all_", seq_year)

# Vocational - males
re_v_m <- paste0("re_VE_m_", seq_year)
p_v_m <- paste0("p_VE_m_", seq_year)
ci_up_v_m <- paste0("ci_up_VE_m_", seq_year)
ci_lo_v_m <- paste0("ci_lo_VE_m_", seq_year)

# Vocational - females
re_v_f <- paste0("re_VE_f_", seq_year)
p_v_f <- paste0("p_VE_f_", seq_year)
ci_up_v_f <- paste0("ci_up_VE_f_", seq_year)
ci_lo_v_f <- paste0("ci_lo_VE_f_", seq_year)

re_p_ci <- c()
for (i in seq_along(re_h)){
  re_p_ci <- c(re_p_ci,
               re_h[i], p_h[i], ci_lo_h[i], ci_up_h[i],
               re_h_m[i], p_h_m[i], ci_lo_h_m[i], ci_up_h_m[i],
               re_h_f[i], p_h_f[i], ci_lo_h_f[i], ci_up_h_f[i],
               re_v[i], p_v[i], ci_lo_v[i], ci_up_v[i],
               re_v_m[i], p_v_m[i], ci_lo_v_m[i], ci_up_v_m[i],
               re_v_f[i], p_v_f[i], ci_lo_v_f[i], ci_up_v_f[i])
}

ncol = length(re_p_ci) + 1
seq_region <- c(seq_region, "Russian Federation")

# Empty dataframe to input results later
RoREs <- as.data.frame((matrix(ncol = ncol, nrow = length(seq_region))))
colnames(RoREs) <-  c("OKATO", re_p_ci)

# Obtaining the values
re_one_region <- c()

for (j in 1:(length(seq_region))){
  for (i in seq_along(seq_year)){
    if (!((j == which(names(Rsmry_all[[1]]) == "Respublika Crimea")|
          j == which(names(Rsmry_all[[1]]) == "Sevastopol")) & 
                     i == 1)
        ){
      #print(j)
      re_one_region <- c(re_one_region,
                         
                     # Higher level
                    (100*(exp(Rsmry_all[[i]][[j]]$coefficients[3,1]) - 1))/4,
                    formatC(Rsmry_all[[i]][[j]]$coefficients[3,4], digits = 2),
                    (100*(exp(confint(Rlm_mincer_all[[i]][[j]])[3,1]) - 1))/4,
                    (100*(exp(confint(Rlm_mincer_all[[i]][[j]])[3,2]) - 1))/4,
                    
                    (100*(exp(Rsmry_m[[i]][[j]]$coefficients[3,1]) - 1))/4,
                    formatC(Rsmry_m[[i]][[j]]$coefficients[3,4], digits = 2),
                    (100*(exp(confint(Rlm_mincer_m[[i]][[j]])[3,1]) - 1))/4,
                    (100*(exp(confint(Rlm_mincer_m[[i]][[j]])[3,2]) - 1))/4,
                    
                    (100*(exp(Rsmry_f[[i]][[j]]$coefficients[3,1]) - 1))/4,
                    formatC(Rsmry_f[[i]][[j]]$coefficients[3,4], digits = 2),
                    (100*(exp(confint(Rlm_mincer_f[[i]][[j]])[3,1]) - 1))/4,
                    (100*(exp(confint(Rlm_mincer_f[[i]][[j]])[3,2]) - 1))/4,
                     
                    # Vocational level
                    (100*(exp(Rsmry_all[[i]][[j]]$coefficients[2,1]) - 1))/3,
                    formatC(Rsmry_all[[i]][[j]]$coefficients[2,4], digits = 2),
                    (100*(exp(confint(Rlm_mincer_all[[i]][[j]])[2,1]) - 1))/3,
                    (100*(exp(confint(Rlm_mincer_all[[i]][[j]])[2,2]) - 1))/3,
                    
                    (100*(exp(Rsmry_m[[i]][[j]]$coefficients[2,1]) - 1))/3,
                    formatC(Rsmry_m[[i]][[j]]$coefficients[2,4], digits = 2),
                    (100*(exp(confint(Rlm_mincer_m[[i]][[j]])[2,1]) - 1))/3,
                    (100*(exp(confint(Rlm_mincer_m[[i]][[j]])[2,2]) - 1))/3,
                    
                    (100*(exp(Rsmry_f[[i]][[j]]$coefficients[2,1]) - 1))/3,
                    formatC(Rsmry_f[[i]][[j]]$coefficients[2,4], digits = 2),
                    (100*(exp(confint(Rlm_mincer_f[[i]][[j]])[2,1]) - 1))/3,
                    (100*(exp(confint(Rlm_mincer_f[[i]][[j]])[2,2]) - 1))/3)   
    }
    else {
      re_one_region <- c(re_one_region, rep(NA, 24))
    }
  }
  RoREs[j,] <- c(seq_region[j], re_one_region)
  re_one_region <- c()
}

RoREs <- cbind.data.frame(Region = c(rgvars[,3], 'Russian Federation'), RoREs)
RoREs$Region <- as.character(RoREs$Region)
RoREs[,-(1:2)] <- lapply(RoREs[,-(1:2)], as.numeric)
# RoREs
#export(RoREs, 'RoREs.xlsx')
# Converting to data.table and melting in order to visualize
# RoREs <- as.data.table(RoREs)

##################################### Plot ##########################################

# 2018
# HE

#temp <- RoREs %>% arrange(re_HE_all_2018)
#RF_color <- ifelse(temp[, "Region"] == "Russian Federation", "red", "black")

#ggplot(data = temp, aes(x = re_HE_all_2018, y = reorder(Region, re_HE_all_2018))) +
#  geom_point(size = 3, color = RF_color) + 
#  geom_errorbarh(aes(xmin = ci_lo_HE_all_2018,
#                     xmax = ci_up_HE_all_2018, height = 0.6)) +
#  theme_bw() +
#  theme(axis.text.y = element_text(color = RF_color, size = 16),
#        axis.text.x = element_text(color = "black", size = 16, face = "bold"),
#        axis.title.y = element_blank(),
#        axis.title.x = element_blank())  +
#  scale_x_continuous(limits = c(-1,85))

#ggsave("reg_he_18.png", width = 8, height = 20,
 #      units = "in")

# VE
#temp <- RoREs %>% arrange(re_VE_all_2018)
#RF_color <- ifelse(temp[, "Region"] == "Russian Federation", "red", "black")

#ggplot(data = temp, aes(x = re_VE_all_2018, y = reorder(Region, re_VE_all_2018))) +
#  geom_point(size = 3, color = RF_color) + 
#  geom_errorbarh(aes(xmin = ci_lo_VE_all_2018,
#                     xmax = ci_up_VE_all_2018, height = 0.6)) +
#  theme_bw() +
#  theme(axis.text.y = element_text(color = RF_color, size = 16),
#        axis.text.x = element_text(color = "black", size = 16, face = "bold"),
#        axis.title.y = element_blank(),
#        axis.title.x = element_blank())  +
#  scale_x_continuous(limits = c(-10, 85))

#ggsave("reg_ve_18.png", width = 8, height = 20,
#       units = "in")

############################################################################################
# A file with region names
Sys.setlocale("LC_CTYPE", "russian")
wd <- "C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3"
setwd(wd)
rgvars <- rio::import("rgvars.xlsx") %>% arrange(OKATO)

colnames(df)[which(colnames(df) == "H00_02")] <- "OKATO"
df$OKATO <- as.character(df$OKATO)
df <- df %>%
  left_join(rgvars, by = "OKATO")

# Descriptive statistics
df$female <- factor(df$female, levels=c(0,1),
                   labels=c("Males",
                            "Females"))

df$KVZV <- df$KVZV/1000

library(tables)
desc_rst <- tabular((Regions = factor(en_rgnames)) ~  (N=1) + 
                       Format(digits=2)*((Wage = wage*(weighted.mean*Arguments(w = KVZV) + sd)) +
                      (Experience = exper*(weighted.mean*Arguments(w = KVZV) + sd))) +
                      (Education = factor(edu_4)*Percent("row")) +
                      (Gender = factor(female)*Percent("row")), data = df)
latex(desc_rst)
# Mixed models

# 2018

library(lme4)
library(lattice)
library(sjPlot)
library(sjmisc)
library(effects)
library(broom) # for glance at AIC, BIC etc.
library(performance) # for icc
library(ggeffects) # for ggpredict
library(margins) # for marginal effects
library(merTools)
library(glmmTMB)

# Null model
M18_0 <- lmer(log(wage) ~ 1 + (1|en_rgnames),
            data = df[df$YEAR == 2018,],
            weights = df[df$YEAR == 2018, "KVZV"],
            control=lmerControl(optimizer="bobyqa"))
# ICC = 0.08875/(0.08875 + 0.46805) # EM I get slightly different numbers

summary(M18_0)
glance(M18_0)
performance::icc(M18_0)

# We have  result that gamma_00 = 10.17810; 
# we have sigma-squared as 0.4493  and tau_00 as 0.0911
# we have rho as 0.169 
# and we have 79 nu_oj coefficients
dotplot(ranef(M18_0),main=F,title="Model M18_0: Random effects - no covariates")
M18_0a <- as.data.frame(ranef(M18_0)[1])


# Adding predictors
M18_1 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) +
                female +   (1|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

# Adding a random effect for edu_4
M18_2 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) +
                female +  (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_1, M18_2) # renef for edu_4 is worth adding!
summary(M18_2)

# I need to get the exper and exper^2 variables defined separately
# for packages like ggeffects - so I use M18_2b

df$s.exper <- scale(df$exper)
df$s.exper.sq <- df$s.exper*df$s.exper
M18_2b <- lmer(log(wage) ~ edu_4 + s.exper + s.exper.sq +
                 female +  (1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

plot_model(M18_2b,type="re",sort.est = "edu_4Higher")
M18_2ba <- get_model_data(M18_2b,type="re",sort.est = "edu_4Higher")

M18_2b_smry <- summary(M18_2b)

# M18_2ba is the long format tidy data for the plot, the random effects
# already have correction for 100(exp(beta) - 1); we do that for the 
# fixed effect part and construct a new dataframe for plotting
# that combines fixed and random effect, with confidence intervals. 

# Returns by region: fized plus region-specific random effect
returns_by_regions <- as.data.frame(matrix(ncol = 1, nrow = 79))

# HE
returns_by_regions$HE_est <- M18_2b_smry$coefficients['edu_4Higher',1] + 
  M18_2ba[M18_2ba$facet == 'edu_4Higher', 'estimate']
# CIs
returns_by_regions$HE_lower <- confint(M18_2b, method="Wald")['edu_4Higher',1] + 
  M18_2ba[M18_2ba$facet == 'edu_4Higher', 'conf.low']
returns_by_regions$HE_upper <- confint(M18_2b, method="Wald")['edu_4Higher',2] + 
  M18_2ba[M18_2ba$facet == 'edu_4Higher', 'conf.high']

# VE
returns_by_regions$VE_est <- M18_2b_smry$coefficients['edu_4Vocational',1] + 
  M18_2ba[M18_2ba$facet == 'edu_4Vocational', 'estimate']
# CIs
returns_by_regions$VE_lower <- confint(M18_2b, method="Wald")['edu_4Vocational',1] + 
  M18_2ba[M18_2ba$facet == 'edu_4Vocational', 'conf.low']
returns_by_regions$VE_upper <- confint(M18_2b, method="Wald")['edu_4Vocational',2] + 
  M18_2ba[M18_2ba$facet == 'edu_4Vocational', 'conf.high']

returns_by_regions <- cbind.data.frame(term = M18_2ba[M18_2ba$facet == 'edu_4Higher','term'],
                                                      returns_by_regions[,-1])

# Calculating returns
# HE
returns_by_regions[,c("HE_est", "HE_lower", "HE_upper")] <- 
  100*(exp(returns_by_regions[,c("HE_est", "HE_lower", "HE_upper")]) - 1)/4
# VE
returns_by_regions[,c("VE_est", "VE_lower", "VE_upper")] <- 
  100*(exp(returns_by_regions[,c("VE_est", "VE_lower", "VE_upper")]) - 1)/3


##################################### Plot ##########################################
priority_regions <- c('Respublika Adygeya', 'Pskovskaya Oblast',
                       'Altayskiy Kray', 'Kurganskaya Oblast',
                       'Respublika Kalmykiya', 'Chuvashskaya Respublika', 
                       'Respublika Altay', 'Respublika Karelia',
                       'Respublika Tyva', 'Respublika Mariy El')

# 2018
# HE
returns_by_regions <- returns_by_regions %>% arrange(HE_est)
col_HE <- ifelse(returns_by_regions$term %in% priority_regions, "red", "black")

ggplot(data = returns_by_regions, aes(x = HE_est, y = reorder(term, HE_est))) +
  geom_point(color = col_HE, size = 3) + 
  geom_errorbarh(aes(xmin = HE_upper,
                     xmax = HE_lower, height = 0.6)) +
  theme_bw() +
  theme(axis.text.y = element_text(color = col_HE, size = 18),
        axis.text.x = element_text(color = "black", size = 18, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())  +
  scale_x_continuous(limits = c(-10,85))

ggsave("reg_he_18.png", width = 8, height = 20,
       units = "in")

# VE
returns_by_regions <- returns_by_regions %>% arrange(VE_est)
col_VE <- ifelse(returns_by_regions$term %in% priority_regions, "red", "black")

ggplot(data = returns_by_regions, aes(x = VE_est, y = reorder(term, VE_est))) +
  geom_point(color = col_VE, size = 3) + 
  geom_errorbarh(aes(xmin = VE_upper,
                     xmax = VE_lower, height = 0.6)) +
  theme_bw() +
  theme(axis.text.y = element_text(color = col_VE, size = 18),
        axis.text.x = element_text(color = "black", size = 18, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())  +
  scale_x_continuous(limits = c(-10,85))


ggsave("reg_ve_18.png", width = 8, height = 20,
        units = "in")



############################################################################################

#################
# Migration: migr
M18_5 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
                scale(migr) + (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_5) # migr is not worth adding
summary(M18_5)

# Cross-level interaction
M18_6 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
                scale(migr)*edu_4 + (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_5, M18_6) # migr does not work as a moderator
summary(M18_6)

#################
# grp
M18_7 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
                scale(grp) + (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_7) # grp is worth adding
summary(M18_7)

# Cross-level interaction
M18_8 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
                 scale(grp)*edu_4 + (1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_7, M18_8) # grp does not work as a moderator
summary(M18_8)

#################
# urban
M18_9 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
                scale(urban) + (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_9) # urban is worth adding
summary(M18_9)

# Cross-level interaction
M18_10 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
                scale(urban)*edu_4 + (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_9, M18_10) # urban does not work as a moderator
summary(M18_10)

#################
# coverage VE
M18_11 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
              scale(cov_VE) + (1 + edu_4|en_rgnames),
              data = df[df$YEAR == 2018,],
              weights = df[df$YEAR == 2018, "KVZV"],
              control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_11) # cov_VE is worth adding
summary(M18_11)
#################
# Cross-level interaction
M18_12 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
               scale(cov_VE)*edu_4 +(1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_11, M18_12) # cov_VE works as a moderator
summary(M18_12)




#################
######## REVISED REVISED 

blix <- getME(M18_12,name=c("Ztlist"))

# df$s.exper <- scale(df$exper)
# df$s.exper.sq <- df$s.exper*df$s.exper
df$s.COV_VE <- scale(df$cov_VE)
df$s.cv.edu_4 <- df$edu_4*df$s.COV_VE

M18_12b <- lmer(log(wage) ~ edu_4 + s.exper + s.exper.sq +
                  female + scale(cov_VE)*edu_4 + (1 + edu_4|en_rgnames),
                data = df[df$YEAR == 2018,],
                weights = df[df$YEAR == 2018, "KVZV"],
                control=lmerControl(optimizer="bobyqa"))

plot_model(M18_12b,type="re",sort.est = "edu_4Higher")
M18_12ba <- get_model_data(M18_12b,type="re",sort.est = "edu_4Higher")

summary(M18_2b)
tab_model(M18_2b,M18_12b)
glance(M18_2b,M18_12b)


#################################
# coverage HE
M18_13 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
                 scale(cov_HE) + (1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_13) # cov_HE is worth adding
summary(M18_13)

# Cross-level interaction
M18_14 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
                 scale(cov_HE)*edu_4 +(1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_13, M18_14) # cov_HE does not work as a moderator
summary(M18_14)

#################
# unemployment
M18_14 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
                 scale(unemploy) + (1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_14) # unemploy is worth adding
summary(M18_14)

# Cross-level interaction
M18_15 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
                 scale(unemploy)*edu_4 +(1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_14, M18_15) # unemploy does not work as a moderator
summary(M18_15)

#################
# nat_res
M18_16 <- lmer(log(wage) ~ edu_4 + scale(exper) + I(scale(exper)^2) + female + 
                 scale(nat_res) + (1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_2, M18_16) # nat_res is worth adding
summary(M18_16)

# Cross-level interaction
M18_17 <- lmer(log(wage) ~ scale(exper) + I(scale(exper)^2) + female + 
                 scale(nat_res)*edu_4 +(1 + edu_4|en_rgnames),
               data = df[df$YEAR == 2018,],
               weights = df[df$YEAR == 2018, "KVZV"],
               control=lmerControl(optimizer="bobyqa"))

anova(M18_16, M18_17) # nat_res does not work as a moderator
summary(M18_17)

#################
# Just for a rough look 
dotplot(coef(M18_2))
dotplot(ranef(M18_2, condVar=T))


#################

# I would like to compare the result of adding fixed and random values to 
# these conditional values or find documentation that they are the same. 

t <- as.data.frame(ranef(M18_2, condVar=T))

# Plotting
ggplot(data = t[t$term == "edu_4Higher",],
       aes(x = condval, y = reorder(grp, -condval))) +
  geom_point(size = 3, color = "darkgreen") + 
  geom_errorbarh(aes(xmin = condval - 1.96*condsd,
                     xmax = condval + 1.96*condsd, height = 0.4)) +
  theme_bw() +
  theme(axis.text.y = element_text(color = "black", size = 12, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 50, hjust = 1, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16))  +
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1.5) +
  xlab("Random effect for higher education")
ggsave("rst_he_18.png", width = 15, height = 7,
       units = "in")


ggplot(data = t[t$term == "edu_4Vocational",],
       aes(x = condval, y = reorder(grp, -condval))) +
  geom_point(size = 3, color = "darkgreen") + 
  geom_errorbarh(aes(xmin = condval - 1.96*condsd,
                     xmax = condval + 1.96*condsd, height = 0.4)) +
  theme_bw() +
  theme(axis.text.y = element_text(color = "black", size = 12, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 50, hjust = 1, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16))  +
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1.5) +
  xlab("Random effect for vocational education")

ggsave("rst_ve_18.png", width = 15, height = 7,
       units = "in")

#############

pub.list <- lmList(log(wage) ~ edu_4 | en_rgnames, 
                   data=df[df$YEAR == 2018,],
                   weights = df[df$YEAR == 2018, "KVZV"])

################### Random effects (was added separately in the main table)

# Extracting random effects for each model
ranef_M18_0 <- as.data.frame(summary(M18_0)$varcor)[1:2, c(1,4)]
ranef_M18_0[,1] <- c("Variance of Intecept",
                     "Residual Deviance")

ranef_M18_1 <- as.data.frame(summary(M18_1)$varcor)[1:2, c(1,4)]
ranef_M18_1[,1] <- c("Variance of Intecept",
                     "Residual Deviance")

ranef_M18_11 <- as.data.frame(summary(M18_11)$varcor)[c(1:3,7), c(1,4)]
ranef_M18_11[,1] <- c("Variance of Intecept",
                      "Variance of Vocational",
                      "Variance of Higher",
                      "Residual Deviance")

ranef_M18_12 <- as.data.frame(summary(M18_12)$varcor)[c(1:3,7), c(1,4)]
ranef_M18_12[,1] <- c("Variance of Intecept",
                      "Variance of Vocational",
                      "Variance of Higher",
                      "Residual Deviance")

# Naming
colnames(ranef_M18_12) <- c("Random Effects", "Value_12")
colnames(ranef_M18_11) <- c("Random Effects", "Value_11")
colnames(ranef_M18_1) <- c("Random Effects", "Value_1")
colnames(ranef_M18_0) <- c("Random Effects", "Value_0")
rownames(ranef_M18_12) <- NULL
rownames(ranef_M18_11) <- NULL
rownames(ranef_M18_1) <- NULL
rownames(ranef_M18_0) <- NULL

# Merging
ranef <- ranef_M18_0 %>%
  full_join(ranef_M18_1, by = "Random Effects")%>%
  full_join(ranef_M18_11, by = "Random Effects")%>%
  full_join(ranef_M18_12, by = "Random Effects")
ranef <- ranef[c(1,3,4,2),]

# Producing the table
colnames(ranef) <- c("", "", "", "", "")
ranef[,-1] <- round(ranef[,-1],2)
#xtable(ranef)
ranef[is.na(ranef[,2]),2 ] <- ""
ranef[is.na(ranef[,3]),3 ] <- ""

#################################### Latex #################################

# Additional model fit criteria

fit.stat <- t(do.call(rbind.data.frame, lapply(c(M18_0, M18_1, M18_11, M18_12), glance)))
fit.stat <- cbind(rownames(fit.stat), round(fit.stat, 3))
################# Fixed effects
stargazer(M18_0,
          M18_1,
          M18_11,
          M18_12,
          type = "latex",
          dep.var.caption = "",
          dep.var.labels.include = F,
          df = F,
          header = F,
          intercept.bottom = F,
          column.labels = c("Null model",
                            "Mincerian",
                            "Random Slope",
                            "Cross-Level Interaction"),
          covariate.labels = c("Constant",
                               "Vocational",
                               "Higher",
                               "Coverage VE X Vocational",
                               "Coverage VE X Higher",
                               "Experience",
                               "Experience squared",
                               "Females",
                               "Coverage VE"),
          add.lines = list(c(as.vector(as.character(ranef[1,]))),
                           c(as.vector(as.character(ranef[2,]))),
                           c(as.vector(as.character(ranef[3,]))),
                           c(as.vector(as.character(ranef[4,]))),
                           c(as.vector(fit.stat[1,])),
                           c(as.vector(fit.stat[5,])),
                           c(as.vector(fit.stat[6,]))))

# Also to add other diagnostics like glance() for each model
################### Specification of the effects for the model with coverage by VE

# Cov_HE_sdandardized = 1, i.e., coverage is high
# computed a raw (non-standardized) value when the standardized one = 1
mean(df$cov_VE, na.rm = T) + 1*sd(df$cov_VE, na.rm = T)

100*(exp(summary(M18_12)$coefficients[6])-1) +
  100*(exp(summary(M18_12)$coefficients[8])-1) # returns for VE
100*(exp(summary(M18_12)$coefficients[7])-1) +
  100*(exp(summary(M18_12)$coefficients[9])-1) # returns for HE

# Cov_HE_sdandardized = 0, i.e., coverage is medium
# computed a raw (non-standardized) value when the standardized one = 0
mean(df$cov_VE, na.rm = T) + 0*sd(df$cov_VE, na.rm = T)

100*(exp(summary(M18_12)$coefficients[6])-1) # returns for VE
100*(exp(summary(M18_12)$coefficients[7])-1) # returns for HE

# Cov_HE_sdandardized = -1, i.e., coverage is low
# computed a raw (non-standardized) value when the standardized one = -1
mean(df$cov_VE, na.rm = T) - 1*sd(df$cov_VE, na.rm = T)

100*(exp(summary(M18_12)$coefficients[6])-1) -
  100*(exp(summary(M18_12)$coefficients[8])-1) # returns for VE
100*(exp(summary(M18_12)$coefficients[7])-1) -
  100*(exp(summary(M18_12)$coefficients[9])-1) # returns for HE


