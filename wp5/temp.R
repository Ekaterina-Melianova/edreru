# Create dataframe with aggregated salary by year, edu level, region and age
df_year_edu_region_age_salary <- df_rosstat %>% group_by(YEAR, edu_4, H00_02, H01_02) %>% summarise(average_salary=weighted.mean(wage, w=KVZV)) %>% filter(edu_4 != "Secondary") %>% ungroup()

df_year_edu_region_age_salary <- df_year_edu_region_age_salary[!(df_year_edu_region_age_salary$H01_02 == 22 & df_year_edu_region_age_salary$edu_4 == "Higher"),]

# Calculate annual wage
df_year_edu_region_age_salary$annual_wage <- df_year_edu_region_age_salary$average_salary*12

# Calculate mean social and private cost in the region
df_colleges_mean_cost <- df_colleges %>% group_by(OKATO) %>%
  summarise(mean_social_cost=mean(social_cost, na.rm=T), mean_private_cost=mean(private_cost, na.rm=T)) %>%
  as.data.frame()
df_universities_mean_cost <- df_universities %>% group_by(OKATO) %>%
  summarise(mean_social_cost=mean(social_cost, na.rm=T), mean_private_cost=mean(private_cost, na.rm=T)) %>%
  as.data.frame()

# Create dataframe with colleges and universities
df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary %>% filter(edu_4 == "Vocational")
df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary %>% filter(edu_4 == "Higher")

# Remove regions which are not present in graduate.edu
df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary_colleges %>% filter(H00_02 %in% df_colleges_mean_cost$OKATO)
df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary_universities %>% filter(H00_02 %in% df_universities_mean_cost$OKATO)
df_universities_mean_cost <- df_universities_mean_cost[!is.na(as.numeric(df_universities_mean_cost$OKATO)),]

df_colleges_mean_cost <- df_colleges_mean_cost[!is.na(as.numeric(df_colleges_mean_cost$OKATO)),]


### Calculate IRR

# 1. Social and Private Returns for Colleges

social_returns_vec <- c()
private_returns_vec <- c()
region_vec <- c()
year_vec <- c()

for (region_okato in unique(df_colleges_mean_cost$OKATO)){
  
  social_cost_temp <- as.numeric(df_colleges_mean_cost %>% filter(OKATO == region_okato) %>% dplyr::select(mean_social_cost))
  private_cost_temp <- as.numeric(df_colleges_mean_cost %>% filter(OKATO == region_okato) %>% dplyr::select(mean_private_cost))
  
  for (year_n in unique(df_year_edu_region_age_salary_colleges$YEAR)){
    
    # Add social cost for 3 years
    social_cost_vec <- rep(-social_cost_temp, 3)
    # Add private cost for 3 years
    private_cost_vec <- rep(-private_cost_temp, 3)
    # Create vector with salary through out the life 
    wage_vec <- (df_year_edu_region_age_salary_colleges %>% filter(YEAR == year_n & H00_02 == region_okato))$annual_wage
    
    # Add foregone earnings
    # social_cost_vec <- social_cost_vec - mean(wage_vec)
    # private_cost_vec <- private_cost_vec - mean(wage_vec)
    
    # Calculate social and private returns
    social_returns_vec <- c(social_returns_vec, FinCal::irr(c(social_cost_vec, wage_vec)))
    private_returns_vec <- c(private_returns_vec, FinCal::irr(c(private_cost_vec, wage_vec)))
    
    region_vec <- c(region_vec, region_okato)
    year_vec <- c(year_vec, year_n)
    
  }
  
}

# Create dataframe with regional returns
df_region_returns_college <- data.frame(region=region_vec,
                                 year=year_vec,
                                 social_returns=social_returns_vec,
                                 private_returns=private_returns_vec)
df_region_returns_college$year <- as.character(df_region_returns_college$year)


# 2. Social and Private Returns for Universities

social_returns_vec <- c()
private_returns_vec <- c()
region_vec <- c()
year_vec <- c()

for (region_okato in unique(df_universities_mean_cost$OKATO)){
  
  social_cost_temp <- as.numeric(df_universities_mean_cost %>% filter(OKATO == region_okato) %>% dplyr::select(mean_social_cost))
  private_cost_temp <- as.numeric(df_universities_mean_cost %>% filter(OKATO == region_okato) %>% dplyr::select(mean_private_cost))
  
  for (year_n in unique(df_year_edu_region_age_salary_universities$YEAR)){
    
    # Add social cost for 3 years
    social_cost_vec <- rep(-social_cost_temp, 3)
    # Add private cost for 3 years
    private_cost_vec <- rep(-private_cost_temp, 3)
    # Create vector with salary through out the life 
    wage_vec <- (df_year_edu_region_age_salary_universities %>% filter(YEAR == year_n & H00_02 == region_okato))$annual_wage
    
    # Add foregone earnings
    # social_cost_vec <- social_cost_vec - mean(wage_vec)
    # private_cost_vec <- private_cost_vec - mean(wage_vec)
    
    # Calculate social and private returns
    social_returns_vec <- c(social_returns_vec, FinCal::irr(c(social_cost_vec, wage_vec)))
    private_returns_vec <- c(private_returns_vec, FinCal::irr(c(private_cost_vec, wage_vec)))
    
    region_vec <- c(region_vec, region_okato)
    year_vec <- c(year_vec, year_n)
    
  }
  
}

# Create dataframe with regional returns
df_region_returns_university <- data.frame(region=region_vec,
                                    year=year_vec,
                                    social_returns=social_returns_vec,
                                    private_returns=private_returns_vec)
df_region_returns_university$year <- as.character(df_region_returns_university$year)

```