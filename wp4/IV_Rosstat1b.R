# IV_Rosstat1b.R

#############################################################################################
#       Defining top 20 and bottom 20 regions based on industrial development measures      #
#############################################################################################

##################### (i) employment in specific industries demand_sum ###################### 
#####################  (ii) the variable empl_voc from the Mirkina data ##################### 

# Demand side variables from Mirkina df
demand_vars <- paste0('chapter_', letters[c(1:4, 7:9)])

# Taking the last year
RoR_15 <- RoR_1990_2015_rub %>% filter(year == 2015) %>% 
  dplyr::select (region, all_of(demand_vars), empl_voc, empl_high) 
RoR_15$demand_sum <- rowSums(RoR_15[,demand_vars])
RoR_15 <- RoR_15 %>% dplyr::select (region, demand_sum, empl_voc, empl_high)
names(RoR_15)[which(names(RoR_15) == 'region')] <- 'RoR_names'

# Merging with the main Rosstat df
df <- df %>% 
  left_join(RoR_15, by = 'RoR_names')

# Filling missings in Nenetskiy Aok by values in Arkhangelskaya Oblast
# since the former is a part of the latter
df[df$en_rgnames == 'Nenetskiy Aok', 'demand_sum'] <- 
  df[df$en_rgnames == 'Arkhangelskaya Oblast', 'demand_sum']
df[df$en_rgnames == 'Nenetskiy Aok', 'empl_voc'] <- 
  df[df$en_rgnames == 'Arkhangelskaya Oblast', 'empl_voc']

#################  (iii) the % of voc ed as final level amongst women 25-64 ################# 

df <- df %>%
  group_by(OKATO) %>%
  mutate(voc_fem_share = sum(edu_4 == "Vocational" & female == 1)/sum(female == 1))

################## (iv) % of women employed in women dominated industries ###################
# already in df from rgvars_2

############# Additional measure
df$empl_voc_by_high <- df$empl_voc/df$empl_high

# Selecting cohorts
df_y <- df[df$H01_02 <= 36,]
df_o <- df[df$H01_02 >= 40 & df$H01_02 <= 51,]

#############################################################################################
#                           Function for running  individual 2sls                           #
#############################################################################################

# Formulas for one by one
fm_tsls1 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + high_n )
fm_tsls2 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + HSGPER)
fm_tsls3 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + s1z)
fm_tsls4 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + migrationrate)
fm_tsls5 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + women2menratio )
fm_tsls6 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + marriagerate )
fm_tsls7 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + fem_ind_prop)
fm_tsls8 <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + Literacy_97)
fm_tsls_all <- c('fm_tsls1', 'fm_tsls2', 'fm_tsls3', 'fm_tsls4', 
                 'fm_tsls5', 'fm_tsls6', 'fm_tsls7', 'fm_tsls8')

# Formulas
fm_OLS <- formula(log(wage) ~ edu_yrs + exper + I(exper^2))
#fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + Literacy_97)

# Subsets for 2SLS
subset_tsls <-  c('df_y[df_y$H01_01 == 2,]', # females younger
                  'df_o[df_o$H01_01 == 2,]', # females older
                  'df_y[df_y$H01_01 == 1,]', # males younger
                  'df_o[df_o$H01_01 == 1,]') # males older

library(AER)
# Function for a series of 2SLS and OLS ONE BY ONE
tsls_ivreg <- function(fm_tsls_all, fm_OLS, subset_tsls){
  # Creating empty list with empty lists for 2SLS
  tsls_ivreg <- list()
  for (i in 1:length(subset_tsls)){
    tsls_ivreg[[i]] <- vector('list', length = length(fm_tsls_all))
  }
  
  # Empty list for OLS
   OLS <- list()
  # running rlassoIVselectZ for each subset (group of interest)
   for (i in 1:length(subset_tsls)){
     for (j in 1:length(fm_tsls_all)){
     tsls_ivreg[[i]][[j]] <- ivreg(fm_tsls_all[j], data = eval(parse(text = paste(subset_tsls[i]))))
     OLS[[i]] <- lm(fm_OLS, data = eval(parse(text = paste(subset_tsls[i]))))
     }
  }
   # resulting list with both tsls_ivreg and OLS
   res <- list(tsls_ivreg, OLS)
   names(res) <- c('tsls_ivreg', 'OLS')
   return(res)
 }

# Running the functions
# One by one 2SLS
tsls_OLS <- tsls_ivreg(fm_tsls = fm_tsls_all, fm_OLS = fm_OLS, subset_tsls = subset_tsls)

#############################################################################################
#                     Aggregating all the parameters for postLasso                          #
#############################################################################################
IV <- rep(c('high_n', 'HSGPER', 's1z', 'migrationrate', 'women2menratio', 
            'marriagerate','fem_ind_prop', 'Literacy_97', 'OLS'), each = 4)

Cohort <- rep(c('Female young', 'Female old', "Male young", 'Male old'), 9)

tsls_OLS_grid <- expand.grid(paste0('summary(tsls_OLS[["tsls_ivreg"]][[', 1:4, ']][['),
                             paste0(1:8, ']])$coefficients[2,c(1,2)]'))

ols_coefs <- rbind(summary(tsls_OLS[["OLS"]][[1]])$coefficients[2,c(1,2)],
                   summary(tsls_OLS[["OLS"]][[2]])$coefficients[2,c(1,2)],
                   summary(tsls_OLS[["OLS"]][[3]])$coefficients[2,c(1,2)],
                   summary(tsls_OLS[["OLS"]][[4]])$coefficients[2,c(1,2)])

tsls_OLS_coef <- eval(parse(text = paste0('cbind(IV, Cohort, rbind(mapply(c,', 
                                          paste0(do.call(paste, c(tsls_OLS_grid, sep="")), collapse = ', '),
                                          ')', ', ols_coefs', '))')))

tsls_OLS_coef <- as.data.frame(tsls_OLS_coef)
names(tsls_OLS_coef)[ncol(tsls_OLS_coef)] <- "SE"

# Estimates as numeric
tsls_OLS_coef$Estimate <- as.numeric(as.character(tsls_OLS_coef$Estimate))
tsls_OLS_coef$SE <- as.numeric(as.character(tsls_OLS_coef$SE))

tsls_OLS_coef[, 3:ncol(tsls_OLS_coef)] <- sapply(3:ncol(tsls_OLS_coef),
                                                 function(i) round(as.numeric(tsls_OLS_coef[,i]), 3))

tsls_OLS_wide <- pivot_wider(tsls_OLS_coef, names_from = Cohort, values_from = c(Estimate, SE))
colnames(tsls_OLS_wide) <- c('IVs', 'Female_young', 'Female_old', "Male_young", 'Male_old',
                             'SE_Female_young', 'SE_Female_old', 'SE_Male_young', 'SE_Male_old')

tsls_OLS_fin <- tsls_OLS_wide %>% group_by(IVs) %>%
  summarise(Female_young = paste0(Female_young,' (', SE_Female_young, ')'),
            Female_old = paste0(Female_old,' (', SE_Female_old, ')'),
            Male_young = paste0(Male_young,' (', SE_Male_young, ')'),
            Male_old = paste0(Male_old,' (', SE_Male_old, ')')) 

tsls_OLS_fin <- tsls_OLS_fin[match(c('high_n', 'HSGPER', 's1z', 'migrationrate', 'women2menratio', 
                                     'marriagerate','fem_ind_prop', 'Literacy_97', 'OLS'),
                                   tsls_OLS_fin$IVs),]


### Number of observations
N <- c()
N_OLS <- c()
# Extracting from the summary list
for (i in 1:4){
  for (j in 1:8){
   N <- c(N, tsls_OLS[["tsls_ivreg"]][[i]][[j]][["n"]])
   }
  N_OLS <- c(N_OLS, tsls_OLS[["OLS"]][[i]][["df.residual"]])
}
N <- cbind.data.frame(N1 = c(N[1:8], N_OLS[1]), N2 = c(N[9:16], N_OLS[2]),
                      N3 = c(N[17:24], N_OLS[3]), N4 = c(N[25:32], N_OLS[4]))

# Additing to the final table
tsls_OLS_fin <- cbind.data.frame(tsls_OLS_fin, N) %>%
  select(IVs, Female_young, N1, Female_old, N2, Male_young, N3, Male_old, N4)

# Latex table with individual 2SLS regressions
library(xtable)
xtable(tsls_OLS_fin)


#############################################################################################
#                                      Creating Ranks                                       #
#############################################################################################

# Removing literacy
df$Literacy_97 <- NULL

##### Ranks
# (i)
df$rank_demand <- dense_rank(-df$demand_sum)
# (ii)
df$rank_empl_voc <- dense_rank(-df$empl_voc)
# (iii)
df$rank_voc_fem <- dense_rank(-df$voc_fem_share)
# (iv)
# df$rank_fem_ind <- dense_rank(-df$fem_ind_prop)

# additional
df$rank_empl_ratio <- dense_rank(-df$empl_voc_by_high)

##### Variables identifying top 20 (1), middle 40 (3), and bottom 20 (3) regions 
# (i)
#df$demand_20  <-  ifelse(df$rank_demand <= 20, 1,
 #                        ifelse(df$rank_demand > (max(df$rank_demand) - 20), 2, 3))
# (ii)
df$empl_voc_20 <- ifelse(df$rank_empl_voc <= 20, 1,
                         ifelse(df$rank_empl_voc > (max(df$rank_empl_voc) - 20), 3, 2))
# (iii)
df$voc_fem_20 <- ifelse(df$rank_voc_fem <= 20, 1,
                        ifelse(df$rank_voc_fem > (max(df$rank_voc_fem) - 20), 3, 2))
# additional
df$empl_ratio_20 <- ifelse(df$rank_empl_ratio <= 20, 1,
                           ifelse(df$rank_empl_ratio > (max(df$rank_empl_ratio) - 20), 3, 2))

# computing mean years of edu by reginal groups
# (i) employment in specific industries
#aggregate(edu_yrs~demand_20, df, mean)
#  (ii) the variable empl_voc
aggregate(edu_yrs~empl_voc_20, df, mean)
#  (iii) the % of voc ed as final level amongst women 25-64
aggregate(edu_yrs~voc_fem_20, df, mean)
# empl_voc multiplied with reciprocal of empl_high
aggregate(edu_yrs~empl_ratio_20, df, mean)

#unique(df[df$voc_fem_20 == 2, 'en_rgnames'])

# Selecting cohorts
df_y <- df[df$H01_02 <= 36,]
df_o <- df[df$H01_02 >= 40 & df$H01_02 <= 51,]

#############################################################################################
#                               Descriptive Statistics                                      #
#############################################################################################

# id for the 4 groups
df$groups <- ifelse(df$H01_02 <= 36 & df$H01_01 == 2, "Female young",
                    ifelse(df$H01_02 >= 40 & df$H01_01 == 2, 'Female older',
                           ifelse(df$H01_02 <= 36 & df$H01_01 == 1, 'Male young',
                                  ifelse(df$H01_02 >= 40 & df$H01_01 == 1, 'Male older', 'peak'))))

table(df$groups)

# removing peak cohort
df_temp <- df[!df$groups == 'peak',]

# Converting to long format
descr <- pivot_longer(df_temp[,c('edu_yrs', 'groups', 'wage', 'empl_voc_20')],
                        cols = c('edu_yrs', 'wage'))

# Generating descriptive stat
descr <- descr %>% group_by(empl_voc_20, name, groups) %>%
  summarise(Mean = mean(value),
            SD = sd(value)) %>%
  pivot_wider(names_from = empl_voc_20, values_from = c(Mean, SD))

# Computing national average
nation <- pivot_longer(df_temp[,c('edu_yrs', 'groups', 'wage')],
                       cols = c('edu_yrs', 'wage'))
nation <- nation %>% group_by(name, groups) %>%
  summarise(Mean = mean(value),
            SD = sd(value)) 

#  Adding national average
descr <- descr %>%
  left_join(nation, by = c('name', 'groups'))

# Rounding
descr[, 3:ncol(descr)] <- round(descr[, 3:ncol(descr)], 2)

# By categories of regions
descr$'Top_20' <- paste0(descr$Mean_1, ' (', descr$SD_1, ')')
descr$'Middle_40' <- paste0(descr$Mean_2, ' (', descr$SD_2, ')')
descr$'Bottom_20' <- paste0(descr$Mean_3, ' (', descr$SD_3, ')')
descr$'Whole_sample' <- paste0(descr$Mean, ' (', descr$SD, ')')

# Selecting only relevant vars
descr <- descr %>% select(name, groups, Top_20, Middle_40, Bottom_20, Whole_sample)

# Adding membership for priority regions
prior_reg <- data.frame('regions' = c('Respublika Adygeya', 'Pskovskaya Oblast',
                       'Altayskiy Kray', 'Kurganskaya Oblast',
                       'Respublika Kalmykiya', 'Chuvashskaya Respublika', 
                       'Respublika Altay', 'Respublika Karelia',
                       'Respublika Tyva', 'Respublika Mariy El'))
# Empty memberhip variable
prior_reg$membership <- NA

# Extracting a category for each priority region
for (i in 1:nrow(priority_regions)){
  prior_reg[i, 'membership'] <- unique(df[which(df$en_rgnames == prior_reg[i,'regions']), 'empl_voc_20'])
}
  
# Aggregating region names
priority_region_names <- c(paste0(prior_reg[prior_reg$membership == 1, 'regions'], collapse = ', '),
  paste0(prior_reg[prior_reg$membership == 2, 'regions'], collapse = ', '),
  paste0(prior_reg[prior_reg$membership == 3, 'regions'], collapse = ', '),
  paste0(prior_reg$regions, collapse = ', '))

# Adding membership to descr
descr['priority_regions', ] <- ""
descr['priority_regions', 'name'] <- 'Priority regions'
descr['priority_regions', 3:ncol(descr)] <- priority_region_names

# naming fixing
rownames(descr) <- NULL
colnames(descr) <- c('Variable', 'Group', 'Top 20', 'Middle 40', 'Bottom 20', 'Whole Sample')

# Latex
xtable(descr)

#############################################################################################
#                Function for running post-Lasso by groups of regions                       #
#############################################################################################

################################## Interactions 
# Creating a combination of instruments for interactions
#iv_cand <- c('high_n', 'HSGPER', 's1z', 'migrationrate', 'women2menratio', 
#           'marriagerate', 'fem_ind_prop')

#iv_cand_grid <- expand.grid(iv_cand, iv_cand, stringsAsFactors = FALSE)
#iv_cand_interact <- paste(do.call(paste, c(iv_cand_grid, sep="*")), collapse = " + ")

# Formulas
#fm_postLasso <- formula(paste("log(wage) ~ ", "edu_yrs + exper + I(exper^2)",
##                              "|", "exper + I(exper^2) + high_n + HSGPER + s1z +
# migrationrate + women2menratio + marriagerate + fem_ind_prop +",
#                              iv_cand_interact, sep = ""))
##################################
fm_postLasso <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                          high_n + HSGPER + s1z + migrationrate + women2menratio + 
                          marriagerate + fem_ind_prop)
# Formulas
fm_OLS <- formula(log(wage) ~ edu_yrs + exper + I(exper^2))
#fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + Literacy_97)


# Subsets for post-Lasso
subset_general <-  c('df_y[df_y$H01_01 == 2 & df_y$', # females younger
                     'df_o[df_o$H01_01 == 2 & df_o$', # females older
                     'df_y[df_y$H01_01 == 1 & df_y$', # males younger
                     'df_o[df_o$H01_01 == 1 & df_o$') # males older

# A vector with our ranking variables (top and bottom)
ranking_vars <- c('empl_ratio_20', 'empl_voc_20', 'voc_fem_20')
values <- c(1,2,3)

# Specific subsets for post-Lasso accounting for the categorization of regions
subset <- c()
for (i in 1:length(ranking_vars)){
  for (j in 1:length(values)){
    subset <- c(subset, paste0(subset_general, ranking_vars[i], ' == ', values[j], ',]'))
  }
}

# Function for a series of postLasso and OLS
postLasso <- function(fm_postLasso, fm_OLS, subset){
  postLasso <- list()
  OLS <- list()
  # running rlassoIVselectZ for each subset (group of interest)
  for (i in 1:length(subset)){
    postLasso[[i]] <- rlassoIVselectZ(fm_postLasso, data = eval(parse(text = paste(subset[i]))))
    OLS[[i]] <- lm(fm_OLS, data = eval(parse(text = paste(subset[i]))))
  }
  # resulting list with both postLasso and OLS
  res <- list(postLasso, OLS)
  names(res) <- c('postLasso', 'OLS')
  return(res)
}

# Running
postLasso_OLS <- postLasso(fm_postLasso = fm_postLasso, fm_OLS = fm_OLS, subset = subset)

#############################################################################################
#                     Aggregating all the parameters for postLasso                          #
#############################################################################################

# Creating identification variables
Region_group <- rep(rep(c('top 20', 'middle 40', 'bottom 20'), each = 4, 3), 2)
Indicator <- rep(rep(ranking_vars, each = 12), 2)
Sample <- rep(rep(c('females', 'males'),  each = 2, 9), 2)
Cohort <- rep(rep(c('Young', 'Older'), 18), 2)
Method <- rep(c('pLasso', 'OLS'), each = 36)
postLasso.coefs <- eval(parse(text = paste0('cbind(Method, Region_group, Indicator, Cohort, Sample,
                                            mapply(c,',
                                            paste0('c(as.numeric(summary(postLasso_OLS$postLasso[[', 1:36, ']])),',
                                                   'confint(postLasso_OLS$postLasso[[', 1:36, ']]))', collapse = ', '), ',',
                                            paste0('c(summary(postLasso_OLS$OLS[[', 1:36, ']])$coefficients["edu_yrs",],',
                                                   'confint(postLasso_OLS$OLS[[', 1:36, ']])["edu_yrs",])', collapse = ', '), '))')))
# Naming
colnames(postLasso.coefs)[6:ncol(postLasso.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'lower', 'upper')
# Rounding
postLasso.coefs[, 6:ncol(postLasso.coefs)] <- sapply(6:ncol(postLasso.coefs),
                                                     function(i) round(as.numeric(postLasso.coefs[,i]), 3))
# A matrix to data.frame
postLasso.coefs <- as.data.frame(postLasso.coefs)

# Estimates as numeric
postLasso.coefs$Est <- as.numeric(as.character(postLasso.coefs$Est))
postLasso.coefs$lower <- as.numeric(as.character(postLasso.coefs$lower))
postLasso.coefs$upper <- as.numeric(as.character(postLasso.coefs$upper))

# A unique identifier Cohort_Sample
postLasso.coefs$cat <- paste0(postLasso.coefs$Method, ' ', postLasso.coefs$Cohort, '_', postLasso.coefs$Sample)
postLasso.coefs$Cohort_Sample <- paste0(postLasso.coefs$Cohort, ' ', postLasso.coefs$Sample)

# Arranging
postLasso.coefs <- postLasso.coefs %>%
  group_by(Method, Region_group, Indicator) %>%
  arrange(Est)

# Splitting the df by method
pLasso_coefs <- postLasso.coefs[postLasso.coefs$Method == 'pLasso',
                                c('Method', 'Region_group', 'Indicator', 'Cohort_Sample', 'Est', 'lower', 'upper')]
OLS_coefs <- postLasso.coefs[postLasso.coefs$Method == 'OLS',
                             c('Method', 'Region_group', 'Indicator', 'Cohort_Sample', 'Est', 'lower', 'upper')]

# Creating a sorted variable for Cohort_Sample
levels <- as.data.frame(pLasso_coefs[pLasso_coefs$Indicator == 'empl_ratio_20' &
                                   pLasso_coefs$Region_group == 'top 20', 'Cohort_Sample'])
pLasso_coefs$cat_sorted <- factor(pLasso_coefs$Cohort_Sample, levels = levels$Cohort_Sample)
OLS_coefs$cat_sorted <- factor(OLS_coefs$Cohort_Sample, levels = levels$Cohort_Sample)

#############################################################################################
#                       Aggregating all the parameters for TSLS                             #
#############################################################################################

# Creating identification variables
#Method <- rep(c('tsls', 'OLS'), each = 64)
#tsls.coefs <- eval(parse(text = paste0('cbind(Method, Region_group, Indicator, Cohort, Sample, mapply(c,',
#                                            paste0('c(as.numeric(tsls_OLS$tsls_ivreg[[', 1:64, ']]$coefficients["edu_yrs"]),',
#                                                   'confint(tsls_OLS$tsls_ivreg[[', 1:64, ']])["edu_yrs",])', collapse = ', '), ',',
 #                                           paste0('c(summary(tsls_OLS$OLS[[', 1:64, ']])$coefficients["edu_yrs",],',
 #                                                  'confint(tsls_OLS$OLS[[', 1:64, ']])["edu_yrs",])', collapse = ', '), '))')))
# Naming
#colnames(tsls.coefs)[6:ncol(tsls.coefs)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'lower', 'upper')
# Rounding
#postLasso.coefs[, 6:ncol(tsls.coefs)] <- sapply(6:ncol(tsls.coefs),
#                                                     function(i) round(as.numeric(tsls.coefs[,i]), 3))
# A matrix to data.frame
#tsls.coefs <- as.data.frame(tsls.coefs)

# Estimates as numeric
#tsls.coefs$Est <- as.numeric(as.character(tsls.coefs$Est))
#tsls.coefs$lower <- as.numeric(as.character(tsls.coefs$lower))
#tsls.coefs$upper <- as.numeric(as.character(tsls.coefs$upper))

# A unique identifier Cohort_Sample
#tsls.coefs$cat <- paste0(tsls.coefs$Method, ' ', tsls.coefs$Cohort, '_', tsls.coefs$Sample)
#tsls.coefs$Cohort_Sample <- paste0(tsls.coefs$Cohort, ' ', tsls.coefs$Sample)

# Arranging
#tsls.coefs <- tsls.coefs %>%
#  group_by(Method, Region_group, Indicator) %>%
#  arrange(Est)

# Splitting the df by method
#tsls_coefs <- tsls.coefs[tsls.coefs$Method == 'tsls',
#                                c('Method', 'Region_group', 'Indicator',
#                                  'Cohort_Sample', 'Est', 'lower', 'upper')]
# Creating a sorted variable for Cohort_Sample
#levels <- as.data.frame(tsls_coefs[tsls_coefs$Indicator == 'demand_10' &
#                                     tsls_coefs$Region_group == 'top 10', 'Cohort_Sample'])
#tsls_coefs$cat_sorted <- factor(tsls_coefs$Cohort_Sample, levels = levels$Cohort_Sample)

#############################################################################################



############################ Merging pLasso and TSLS (on literacy), OLS keep only once (from pLasso)
pLasso_TSLS_coefs <- rbind.data.frame(pLasso_coefs,  OLS_coefs)

# Plotting coefficients

# demand_20
#ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'demand_20',],
#       aes(y = cat_sorted, colour = Method)) + 
#  geom_errorbar(aes(xmin = lower, xmax = upper),
#                width = 0, size = 1) + 
#  scale_color_manual(values = c('darkgreen', 'blue'), 
#                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate')) +
#  geom_point(aes(x = Est, shape = Region_group), size = 4) +
#  scale_shape_manual(values=c(25, 17)) +
#  theme(panel.background = element_blank(),
#        axis.title = element_blank(),
#        axis.text = element_text(size = 14, face = 'bold'), 
#        axis.line = element_line(color = 'black'),
#        plot.title.position = 'plot') +
#  ggtitle('Employment in specific industries')


# voc_fem_20
ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'voc_fem_20',],
       aes(y = cat_sorted)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, color = Method),
                width = 0, size = 2) + 
  scale_color_manual(values = c('darkgreen', 'red'), 
                     labels = c('OLS', 'pLasso')) +
  geom_point(aes(x = Est, shape = Region_group, fill = Method), size = 7,
             stroke = 1, color = 'black') + #Levels: bottom 20 middle 40 top 20
  scale_shape_manual(values = c(25, 22, 24)) +
  scale_fill_manual(values = c('darkgreen', 'red'), 
                    labels = c('OLS', 'pLasso'), guide = 'none') +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 25, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot',
        legend.position="none") +
 # ggtitle('the % of voc ed as final level amongst women 25-64')+
  geom_vline(xintercept = c(0.2, 0.4, 0.6), color = 'grey')

ggsave("iv_2_methods_1.png", width = 15, height = 6,
       units = "in")

# empl_voc_20
ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'empl_voc_20',],
            aes(y = cat_sorted)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, color = Method),
                width = 0, size = 2) + 
  scale_color_manual(values = c('darkgreen', 'red'), 
                     labels = c('OLS', 'pLasso')) +
  geom_point(aes(x = Est, shape = Region_group, fill = Method), size = 7,
             stroke = 1, color = 'black') + #Levels: bottom 20 middle 40 top 20
  scale_shape_manual(values = c(25, 22, 24)) +
  scale_fill_manual(values = c('darkgreen', 'red'), 
                    labels = c('OLS', 'pLasso'), guide = 'none') +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 25, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot',
        legend.position="none") +
  #ggtitle('empl_voc from the Mirkina data') +
  geom_vline(xintercept = c(0.2, 0.4, 0.6), color = 'grey')

ggsave("iv_2_methods_2.png", width = 15, height = 6,
       units = "in")

# empl_ratio_20
ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'empl_ratio_20',],
       aes(y = cat_sorted))  +
  geom_point(aes(x = Est, shape = Region_group, fill = Method), size = 7,
             stroke = 1) + #Levels: bottom 20 middle 40 top 20
  scale_shape_manual(values = c(25, 22, 24)) + 
  scale_color_manual(values = c('darkgreen', 'red'), 
                     labels = c('OLS', 'pLasso')) +
  scale_fill_manual(values = c('darkgreen', 'red'), 
                    labels = c('OLS', 'pLasso'), guide = 'none')  + 
  geom_errorbar(aes(xmin = lower, xmax = upper, color = Method),
                width = 0, size = 2)+
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 25, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot',
        legend.position = 'bottom',
        legend.text = element_text(colour = "black", size = 20),
        legend.title = element_text(colour = "black", size = 20, face = "bold")) +
  #ggtitle('empl_voc multiplied with reciprocal of empl_high') +
  geom_vline(xintercept = c(0.2, 0.4, 0.6), color = 'grey')

ggsave("iv_2_methods_3.png", width = 15, height = 6,
       units = "in")

#gridExtra::grid.arrange(a, b, c, nrow = 3, ncol = 1)


# fem_ind_20
#ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'fem_ind_20',],
#       aes(y = cat_sorted, colour = Method)) + 
#  geom_errorbar(aes(xmin = lower, xmax = upper),
#                width = 0, size = 1) + 
#  scale_color_manual(values = c('darkgreen', 'blue'), 
#                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate')) +
#  geom_point(aes(x = Est, shape = Region_group), size = 4) +
#  scale_shape_manual(values=c(25, 17)) +
#  theme(panel.background = element_blank(),
#        axis.title = element_blank(),
#        axis.text = element_text(size = 14, face = 'bold'), 
#        axis.line = element_line(color = 'black'),
#        plot.title.position = 'plot') +
#  ggtitle('% of women employed in women dominated industries')
