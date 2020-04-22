# IV_RLMS2a.R

########################################### Data  ################################################

### Main RLMS data
rlms <- readRDS('C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp1/df_mincer.rds')

# Filtering 2018 
rlms18 <- filter(rlms, YEAR == 2018)

# Some functions -later to be edreru package
source("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/edreru_package.R")

# Region
Region_rlms <- selectFromSQL(c("IDIND", "YEAR", "Region", "AGE")) %>% filter(YEAR == 2018)

# Merging
rlms18 <- rlms18 %>%
  left_join(Region_rlms[,c("IDIND", "YEAR", "REGION", "AGE")], by = c("IDIND", "YEAR"))

# Literacy 1897
Grig <- rio::import('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/Grigoriev.xlsx')
names(Grig)[4] <- 'OKATO'
names(Grig)[5] <- 'REGION'

# Merging with the main df
rlms18 <- rlms18 %>%
  left_join(Grig[, c('Literacy_97', 'OKATO', 'REGION')], by = 'REGION')

# IVs
Sys.setlocale("LC_CTYPE", "russian")
# load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/EGE1d.rdata')
# load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/EGE1e.rdata')
# load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/PISA01.rdata')
load('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/qq1.rdata')
RoR_1990_2015_rub <- readstata13::read.dta13('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Regional/RoR_1990_2015_rub.dta')
rgvars <- import("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/rgvars.xlsx")
rgvars_2 <- import("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/rgvars_2.xlsx")

# Taking last year
RoR_15 <- RoR_1990_2015_rub %>% filter(year == 2015)
names(RoR_15)[which(names(RoR_15) == 'region')] <- 'RoR_names'

###### Potential IV

#### QUANTITY
# high_n : number of HE institutions
high_n <- RoR_15[, c('RoR_names', 'high_n')]

# HSGPER: High School graduate students per school
HSGPER <- qq1[, c('region', 'HSGPER')]
names(HSGPER)[1] <- 'RoR_names'

### QUALITY
# EGE
EGE <- rgvars[, c('RoR_names', 's1z', 'OKATO', 'districts')] %>% drop_na()

### MIGRATION
# migrationrate: Net migration rate (from ROR Stata file)
migrationrate <- RoR_15[, c('RoR_names', 'migrationrate')]

### WOMEN
# women2menratio
women2menratio <- RoR_15[, c('RoR_names', 'women2menratio')]

# marriagerate
marriagerate <- RoR_15[, c('RoR_names', 'marriagerate')]

# fem_industry share
fem_ind_prop <- rgvars_2[, c('RoR_names', 'fem_ind_prop')] %>% drop_na()

# all IV candidates
ivs <- high_n %>%
  left_join(HSGPER, by = 'RoR_names') %>%
  left_join(EGE, by = 'RoR_names') %>%
  left_join(migrationrate, by = 'RoR_names') %>%
  left_join(women2menratio, by = 'RoR_names') %>%
  left_join(marriagerate, by = 'RoR_names') %>%
  left_join(fem_ind_prop, by = 'RoR_names')

# Adding them to the RLMS df

# Merging
rlms18 <- rlms18 %>%
  left_join(ivs, by = 'OKATO')

# Adding transformed vars
rlms18$lnwage <- log(rlms18$wage)
rlms18$exper2 <- (rlms18$exper)^2

##################################################################################################3
# Analysis

fm_postLasso <- formula(log(wage) ~ edu_yrs + exper + I(exper^2)|exper + I(exper^2) + high_n + HSGPER + s1z +
                          migrationrate + women2menratio + marriagerate + fem_ind_prop)

pLasso_whole <- rlassoIVselectZ(fm_postLasso, data = rlms18)
pLasso_whole$selected

