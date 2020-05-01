# IV_Rosstat1a_.R

# Separate IV analysis for 2015 data

Rosstat15 <- readRDS('C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/Rosstat15.rds')
names(Rosstat15)[1] <- 'OKATO'

# fem_industry share
fem_ind_prop <- rgvars_2[, c('RoR_names', 'fem_ind_prop', 'OKATO')] %>% drop_na()

# all IV candidates
ivs <- high_n %>%
  left_join(HSGPER, by = 'RoR_names') %>%
  left_join(EGE, by = 'RoR_names') %>%
  left_join(migrationrate, by = 'RoR_names') %>%
  left_join(women2menratio, by = 'RoR_names') %>%
  left_join(marriagerate, by = 'RoR_names')%>%
  left_join(fem_ind_prop, by = c('RoR_names', 'OKATO'))

# Merging
df <- Rosstat15 %>%
  left_join(ivs, by = 'OKATO')

df <- na.omit(df)

# Literacy 1897
Grig <- rio::import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.xlsx')
names(Grig)[4] <- 'OKATO'

# Merging with the main df
df <- df %>%
  left_join(Grig[, c('Literacy_97', 'OKATO')], by = 'OKATO')

# Adding transformed vars
df$lnwage <- log(df$wage)
df$exper2 <- (df$exper)^2

#export(df, 'Rosstat15.dta')
########################################################################################

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + high_n)

ivreg_whole1_15 <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + HSGPER)

ivreg_whole2_15  <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + s1z)

ivreg_whole3_15  <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + migrationrate)

ivreg_whole4_15  <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + women2menratio)

ivreg_whole5_15  <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + marriagerate)

ivreg_whole6_15  <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + fem_ind_prop)

ivreg_whole7_15  <- ivreg(fm_tsls, data = df)

#
fm_tsls <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + Literacy_97)

ivreg_whole8_15  <- ivreg(fm_tsls, data = df)

# OLS
Rosstat_ols_whole_15 <- lm(log(wage) ~ edu_yrs + exper + I(exper^2), data = Rosstat15, 
                           weights = Rosstat15$KVZV)

