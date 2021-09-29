# rev2.R

library(dplyr)
library(foreign)

######################################### Data ###########################################################



rst_19 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_19i.sav",
                     use.value.labels = F,
                     use.missings=TRUE,
                     to.data.frame = TRUE)
rst_18 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_18.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_17 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_17.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_16 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_16.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_15 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_15.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)
rst_14 <- read.spss(file="C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/ROSSTAT/rosstat_14.sav",
                    use.value.labels = F,
                    use.missings=TRUE,
                    to.data.frame = TRUE)


df_19 <- rst_19 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2019)
df_18 <- rst_18 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2018)
df_17 <- rst_17 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2017)
df_16 <- rst_16 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2016)
df_15 <- rst_15 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2015)
df_14 <- rst_14 %>% dplyr::select(H00_02, H00_04, H01_00, H01_02, I01_10,
                                  R_DEN, H01_01, VZR_RAB, KVZV) %>% mutate (YEAR = 2014)


#  Adjust for the prices in 2016
df_14$R_DEN <- df_14$R_DEN * 1.237
df_15$R_DEN <- df_15$R_DEN * 1.071
df_16$R_DEN <- df_16$R_DEN * 1
df_17$R_DEN <- df_17$R_DEN * 0.96
df_18$R_DEN <- df_18$R_DEN * 0.94
df_19$R_DEN <- df_19$R_DEN * 0.90


df_ <- rbind(df_14, df_15, df_16, df_17, df_18, df_19)



# No Filtering age
# df <- df_[df_$H01_02 >= 22 & df_$H01_02 < 65,]

# Filtering employed
df <- df_[!is.na(df_$VZR_RAB),]

# Education 

# 4 categories:
# 0 - lower than secondary
# 1 - secondary 
# 2 - specialized / vocational
# 3 - higher and above

df$edu_4 <- car::recode(df$I01_10, "9=0; 7:8=1; 5:6=2; 1:4=3")


# Filtering 3 education levels
df <- df[df$edu_4>0,]

# Education as factor
df$edu_4 <- factor(df$edu_4, levels=c(1,2,3),
                   labels=c("Secondary",
                            "Vocational",
                            "Higher"))

# Wage
df$wage <- df$R_DEN/12

# Filtering wage > 0 
df <- df %>%
  filter(wage >0)

# Socio-demographics
# Gender
df$female[df$H01_01==2] <- 1
df$female[df$H01_01==1] <- 0


# Experience (naive)
df$edu_yrs <- car::recode(df$I01_10, "1=20; 2=17; 3=16; 4=14; 5=12;
                            6=11; 7=11; 8=9")
df$exper <- df$H01_02 - df$edu_yrs - 6
df$exper <- ifelse(df$exper < 0, 0, df$exper)

df_rosstat <- df


# Create dataframe with aggregated salary by year, edu level, region and age
df_year_edu_region_age_salary <- df_rosstat %>% group_by(YEAR, edu_4, H00_02, H01_02) %>% summarise(average_salary=weighted.mean(wage, w=KVZV)) %>% filter(edu_4 != "Secondary") %>% ungroup()

df_year_edu_region_age_salary <- df_year_edu_region_age_salary[!(df_year_edu_region_age_salary$H01_02 == 22 & df_year_edu_region_age_salary$edu_4 == "Higher"),]
# Calculate annual wage
df_year_edu_region_age_salary$annual_wage <- df_year_edu_region_age_salary$average_salary*12


# Create dataframe with colleges and universities
df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary %>% filter(edu_4 == "Vocational")
df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary %>% filter(edu_4 == "Higher")

# Remove regions which are not present in graduate.edu
df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary_colleges %>% filter(H00_02 %in% df_colleges_mean_cost$OKATO)
df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary_universities %>% filter(H00_02 %in% df_universities_mean_cost$OKATO)


#save(df_year_edu_region_age_salary_colleges, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_colleges.rda")
#save(df_year_edu_region_age_salary_universities,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")

load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")

glimpse(df_year_edu_region_age_salary_universities)

range(df_year_edu_region_age_salary_universities$H01_02)

df1 <- df_year_edu_region_age_salary_universities

a14 <- df1 %>% filter(YEAR==2014) %>% transmute(OKATO=H00_02,age=H01_02,aw=round(annual_wage,0))


z14 <- df_universities %>%  filter(OKATO=="01")

z14[,13:15]

z14[,"graduate_years"]

# grads in 2013 

# 240562  235924 237756
# Age 27 
# Uni 1
# Age 28 240562 306325 0.79
# Age 29 235924 186822 1.26
# Age 30 237756 345059 0.69
# Uni 2
# Age 28 297548 306325 0.97
# Age 29 344354 186822 1.84
# Age 30 361836 345059 1.05

240562/306325
235924/186822
237756/345059
# Uni 2
297548/306325
344354/186822
361836/345059






