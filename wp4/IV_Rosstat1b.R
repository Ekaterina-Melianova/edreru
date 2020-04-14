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

#############
df$empl_voc_by_high <- df$empl_voc/df$empl_high

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
df$rank_fem_ind <- dense_rank(-df$fem_ind_prop)

# additional
df$rank_empl_ratio <- dense_rank(-df$empl_voc_by_high)

##### Variables identifying top 10 (1) and bottom 10 (2) regions, else == 3
# (i)
df$demand_20  <-  ifelse(df$rank_demand <= 20, 1,
                         ifelse(df$rank_demand > (max(df$rank_demand) - 20), 2, 3))
# (ii)
df$empl_voc_20 <- ifelse(df$rank_empl_voc <= 20, 1,
                         ifelse(df$rank_empl_voc > (max(df$rank_empl_voc) - 20), 2, 3))
# (iii)
df$voc_fem_20 <- ifelse(df$rank_voc_fem <= 20, 1,
                        ifelse(df$rank_voc_fem > (max(df$rank_voc_fem) - 20), 2, 3))
# (iv)
df$fem_ind_20 <- ifelse(df$rank_fem_ind <= 20, 1,
                        ifelse(df$rank_fem_ind > (max(df$rank_fem_ind) - 20), 2, 3))

# additional
df$empl_ratio_20 <- ifelse(df$rank_empl_ratio <= 20, 1,
                        ifelse(df$rank_empl_ratio > (max(df$rank_empl_ratio) - 20), 2, 3))

# computing mean years of edu by reginall groups
# (i) employment in specific industries
aggregate(edu_yrs~demand_20, df, mean)
#  (ii) the variable empl_voc
aggregate(edu_yrs~empl_voc_20, df, mean)
#  (iii) the % of voc ed as final level amongst women 25-64
aggregate(edu_yrs~voc_fem_20, df, mean)
# (iv) % of women employed in women dominated industries
aggregate(edu_yrs~fem_ind_20, df, mean)
# empl_voc multiplied with reciprocal of empl_high
aggregate(edu_yrs~empl_ratio_20, df, mean)

unique(df[df$voc_fem_20 == 2, 'en_rgnames'])

# Selecting younger cohort
df_y <- df[df$H01_02 <= 36,]
df_o <- df[df$H01_02 >= 40 & df$H01_02 <= 51,]

#############################################################################################
#                Functions for running postLasso and 2sls by districts                      #
#############################################################################################

# Creating a combination of instruments for interactions
iv_cand <- c('high_n', 'HSGPER', 's1z', 'migrationrate', 'women2menratio', 
           'marriagerate', 'fem_ind_prop')

iv_cand_grid <- expand.grid(iv_cand, iv_cand, stringsAsFactors = FALSE)

# Interactions 
iv_cand_interact <- paste(do.call(paste, c(iv_cand_grid, sep="*")), collapse = " + ")

# Formulas
fm_postLasso <- formula(paste("log(wage) ~ ", "edu_yrs + exper + I(exper^2)",
                              "|", "exper + I(exper^2) + high_n + HSGPER + s1z + migrationrate + women2menratio + marriagerate + fem_ind_prop +",
                              iv_cand_interact, sep = ""))
                        
                        
                        
                        
# Formulas
fm_postLasso <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                          high_n + HSGPER + s1z + migrationrate + women2menratio + 
                          marriagerate + fem_ind_prop)
fm_OLS <- formula(log(wage) ~ edu_yrs + exper + I(exper^2))
#fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + Literacy_97)

# Subsets
subset_general <-  c('df_y[df_y$H01_01 == 1 & df_y$', # females younger
                     'df_o[df_o$H01_01 == 1 & df_o$', # females older
                     'df_y[df_y$H01_01 == 1 & df_y$married == 1 & df_y$', # females younger married
                     'df_o[df_o$H01_01 == 1 & df_o$married == 1 & df_o$', # females older married
                     'df_y[df_y$H01_01 == 1 & df_y$married == 0 & df_y$', # females younger signle
                     'df_o[df_o$H01_01 == 1 & df_o$married == 0 & df_o$', # females older single
                     'df_y[df_y$H01_01 == 2 & df_y$', # males younger
                     'df_o[df_o$H01_01 == 2 & df_o$') # males older

# A vector with our ranking variables (top and bottom)
ranking_vars <- c('empl_ratio_20', 'empl_voc_20', 'voc_fem_20', 'fem_ind_20')
values <- c(1,2)

# Specific subsets
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

# Function for a series of 2SLS and OLS
#tsls_ivreg <- function(fm_tsls, fm_OLS, subset){
#  tsls_ivreg <- list()
 # OLS <- list()
  # running rlassoIVselectZ for each subset (group of interest)
#  for (i in 1:length(subset)){
#    tsls_ivreg[[i]] <- ivreg(fm_tsls, data = eval(parse(text = paste(subset[i]))))
 #   OLS[[i]] <- lm(fm_OLS, data = eval(parse(text = paste(subset[i]))))
 # }
  # resulting list with both tsls_ivreg and OLS
 # res <- list(tsls_ivreg, OLS)
 # names(res) <- c('tsls_ivreg', 'OLS')
 # return(res)
#}

# Running the functions
postLasso_OLS <- postLasso(fm_postLasso = fm_postLasso, fm_OLS = fm_OLS, subset = subset)
#tsls_OLS <- tsls_ivreg(fm_tsls = fm_tsls, fm_OLS = fm_OLS, subset = subset)

#############################################################################################
#                     Aggregating all the parameters for postLasso                          #
#############################################################################################

# Creating identification variables
Region_group <- rep(rep(c('top 20', 'bottom 20'), each = 8, 4), 2)
Indicator <- rep(rep(ranking_vars, each = 16), 2)
Sample <- rep(rep(c('females all', 'females married', 'females single', 'males'),  each = 2, 8), 2)
Cohort <- rep(rep(c('Young', 'Older'), 32), 2)
Method <- rep(c('pLasso', 'OLS'), each = 64)
postLasso.coefs <- eval(parse(text = paste0('cbind(Method, Region_group, Indicator, Cohort, Sample,
                                            mapply(c,',
                                            paste0('c(as.numeric(summary(postLasso_OLS$postLasso[[', 1:64, ']])),',
                                                   'confint(postLasso_OLS$postLasso[[', 1:64, ']]))', collapse = ', '), ',',
                                            paste0('c(summary(postLasso_OLS$OLS[[', 1:64, ']])$coefficients["edu_yrs",],',
                                                   'confint(postLasso_OLS$OLS[[', 1:64, ']])["edu_yrs",])', collapse = ', '), '))')))
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
#                                            paste0('c(summary(tsls_OLS$OLS[[', 1:64, ']])$coefficients["edu_yrs",],',
#                                                   'confint(tsls_OLS$OLS[[', 1:64, ']])["edu_yrs",])', collapse = ', '), '))')))
## Naming
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
       aes(y = cat_sorted, colour = Method)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, size = 1) + 
  scale_color_manual(values = c('darkgreen', 'blue'), 
                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate')) +
  geom_point(aes(x = Est, shape = Region_group), size = 4) +
  scale_shape_manual(values=c(25, 17)) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot') +
  ggtitle('the % of voc ed as final level amongst women 25-64')

# fem_ind_20
ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'fem_ind_20',],
       aes(y = cat_sorted, colour = Method)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, size = 1) + 
  scale_color_manual(values = c('darkgreen', 'blue'), 
                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate')) +
  geom_point(aes(x = Est, shape = Region_group), size = 4) +
  scale_shape_manual(values=c(25, 17)) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot') +
  ggtitle('% of women employed in women dominated industries')

# empl_voc_20
a <- ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'empl_voc_20',],
            aes(y = cat_sorted, colour = Method)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, size = 1) + 
  scale_color_manual(values = c('darkgreen', 'blue'), 
                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate')) +
  geom_point(aes(x = Est, shape = Region_group), size = 4) +
  scale_shape_manual(values=c(25, 17)) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot') +
  ggtitle('empl_voc from the Mirkina data') +
  geom_vline(xintercept = c(0.2, 0.4, 0.6), color = 'grey')

# empl_ratio_20
b <- ggplot(pLasso_TSLS_coefs[pLasso_TSLS_coefs$Indicator == 'empl_ratio_20',],
       aes(y = cat_sorted, colour = Method)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, size = 1) + 
  scale_color_manual(values = c('darkgreen', 'blue'), 
                     labels = c('OLS', 'pLasso: high_n, HSGPER, migrationrate')) +
  geom_point(aes(x = Est, shape = Region_group), size = 4) +
  scale_shape_manual(values=c(25, 17)) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot') +
  ggtitle('empl_voc multiplied with reciprocal of empl_high') +
  geom_vline(xintercept = c(0.2, 0.4, 0.6), color = 'grey')

gridExtra::grid.arrange(a, b, nrow = 2, ncol = 1)

