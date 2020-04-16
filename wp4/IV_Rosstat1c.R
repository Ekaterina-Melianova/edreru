# IV_Rosstat1c.R

#################################### Post-Lasso by districts ###############################

fm_postLasso <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) +
                          high_n + HSGPER + s1z + migrationrate + women2menratio + 
                          marriagerate + fem_ind_prop)
# Formulas
fm_OLS <- formula(log(wage) ~ edu_yrs + exper + I(exper^2))

# Subsets for post-Lasso
subset_general <-  c('df_y[df_y$H01_01 == 2 & df_y$', # females younger
                     'df_o[df_o$H01_01 == 2 & df_o$', # females older
                     'df_y[df_y$H01_01 == 1 & df_y$', # males younger
                     'df_o[df_o$H01_01 == 1 & df_o$') # males older

# A vector with our ranking variables (top and bottom)
values <- c('Central', 'Far Eastern', 'North Caucasian', 'Northern',
             'Siberia', 'Southern', 'Ural', 'Volga')

# Specific subsets for post-Lasso accounting for the categorization of regions
subset <- c()
for (j in 1:length(values)){
   subset <- c(subset, paste0(subset_general, 'districts',
                              ' == ', ' "', values[j], '" ', ',]'))
}

# Running
postLasso_OLS_distr <- postLasso(fm_postLasso = fm_postLasso, fm_OLS = fm_OLS, subset = subset)

#############################################################################################
#                     Aggregating all the parameters for postLasso                          #
#############################################################################################

# Creating identification variables
Region_group <- rep(rep(values, each = 4), 2)
Sample <- rep(rep(c('females', 'males'),  each = 2, 8), 2)
Cohort <- rep(rep(c('Young', 'Older'), 16), 2)
Method <- rep(c('pLasso', 'OLS'), each = 32)
postLasso.coefs_distr <- eval(parse(text = paste0('cbind(Method, Region_group, Cohort, Sample,
                                            mapply(c,',
                                            paste0('c(as.numeric(summary(postLasso_OLS_distr$postLasso[[', 1:32, ']])),',
                                                   'confint(postLasso_OLS_distr$postLasso[[', 1:32, ']]))', collapse = ', '), ',',
                                            paste0('c(summary(postLasso_OLS_distr$OLS[[', 1:32, ']])$coefficients["edu_yrs",],',
                                                   'confint(postLasso_OLS_distr$OLS[[', 1:32, ']])["edu_yrs",])', collapse = ', '), '))')))
# Naming
colnames(postLasso.coefs_distr)[5:ncol(postLasso.coefs_distr)] <- c('Est', 'SE', 'test_stat', 'pvalue', 'lower', 'upper')
# Rounding
postLasso.coefs_distr[, 5:ncol(postLasso.coefs_distr)] <- sapply(5:ncol(postLasso.coefs_distr),
                                                     function(i) round(as.numeric(postLasso.coefs_distr[,i]), 3))
# A matrix to data.frame
postLasso.coefs_distr <- as.data.frame(postLasso.coefs_distr)

# Estimates as numeric
postLasso.coefs_distr$Est <- as.numeric(as.character(postLasso.coefs_distr$Est))
postLasso.coefs_distr$lower <- as.numeric(as.character(postLasso.coefs_distr$lower))
postLasso.coefs_distr$upper <- as.numeric(as.character(postLasso.coefs_distr$upper))

# A unique identifier Cohort_Sample
postLasso.coefs_distr$cat <- paste0(postLasso.coefs_distr$Method, ' ', 
                                    postLasso.coefs_distr$Cohort, '_', postLasso.coefs_distr$Sample)
postLasso.coefs_distr$Cohort_Sample <- paste0(postLasso.coefs_distr$Cohort, ' ', postLasso.coefs_distr$Sample)

# Arranging
postLasso.coefs_distr <- postLasso.coefs_distr %>%
  group_by(Method, Region_group) %>%
  arrange(Est)

# Splitting the df by method
pLasso_coefs <- postLasso.coefs_distr[postLasso.coefs_distr$Method == 'pLasso',
                                c('Method', 'Region_group', 'Cohort_Sample', 'Est', 'lower', 'upper')]
OLS_coefs <- postLasso.coefs_distr[postLasso.coefs_distr$Method == 'OLS',
                             c('Method', 'Region_group', 'Cohort_Sample', 'Est', 'lower', 'upper')]

# Creating a sorted variable for Cohort_Sample
levels <- as.data.frame(pLasso_coefs[pLasso_coefs$Region_group == 'Central', 'Cohort_Sample'])
pLasso_coefs$cat_sorted <- factor(pLasso_coefs$Cohort_Sample, levels = levels$Cohort_Sample)
OLS_coefs$cat_sorted <- factor(OLS_coefs$Cohort_Sample, levels = levels$Cohort_Sample)

pLasso_TSLS_coefs <- rbind.data.frame(pLasso_coefs,  OLS_coefs)

################ Selecting extremes

temp <- pLasso_TSLS_coefs %>% group_by(Method, Cohort_Sample) %>%
  mutate(min = ifelse(Est == min(Est), 1,0),
         max = ifelse(Est == max(Est), 1,0))

pLasso_TSLS_coefs_extremes <- temp[!(temp$min == 0 & temp$max == 0),]
  
# Plotting 
ggplot(pLasso_TSLS_coefs_extremes,
       aes(y = cat_sorted)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, color = Method),
                width = 0, size = 3)+
  #Levels: bottom 20 middle 40 top 20
  scale_shape_manual(values = c(1,4,5,14)) +
  scale_fill_manual(values = c('darkgreen', 'red'), 
                    labels = c('OLS', 'pLasso'), guide = 'none')   +
  geom_point(aes(x = Est, shape = Region_group,  fill = Method), size = 7,
             stroke =2) + 
  scale_color_manual(values = c('darkgreen', 'red'), 
                     labels = c('OLS', 'pLasso')) +
  geom_text(aes(x = Est, label = Est, vjust = -1), size = 6, color="black" ) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 25, face = 'bold'), 
        axis.line = element_line(color = 'black'),
        plot.title.position = 'plot',
        legend.position = 'bottom',
        legend.text = element_text( size = 18),
        legend.title = element_text( size = 18, face = "bold")) +
  # ggtitle('the % of voc ed as final level amongst women 25-64')+
  geom_vline(xintercept = c(0.2, 0.4, 0.6), color = 'grey')+
  theme(legend.background = element_rect(color = 0)) 

ggsave("iv_by_districts.png", width = 15, height = 7,
       units = "in")
