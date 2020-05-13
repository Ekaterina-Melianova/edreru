#SP_1a.R

# Table of regional returns

library(dplyr)
library(openxlsx)

glimpse(df_region_returns_university_full)


mrs <- read.xlsx("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3/RoREs_cleaned.xlsx")


dfc_ <- left_join(df_region_returns_college ,mrs,by=c("region"="OKATO"))

dfc_$pr_coll <- dfc_$private_returns*100
dfc_$sr_coll <- dfc_$social_returns*100


temp <- df_region_returns_university %>% 
          dplyr::select(region,sru=social_returns, pru=private_returns)

dfcu_ <- left_join(dfc_,temp,by="region")

dfcu <- dfcu_ %>% transmute(en_rgnames=en_rgnames,
                            mr_coll=re_VE_all_2018,
                            mr_univ=re_HE_all_2018,
                            pr_coll=pr_coll,
                            pr_univ=pru*100,
                            sr_coll=sr_coll,
                            sr_univ=sru*100) 

library(tables)
#dfcu_t <- tabular((Regions= factor(en_rgnames)) ~ (N=1)+ 
#                    Format(digits=2)*((M_college = mr_coll*(weighted.mean*Arguments(w=1)+ sd))
#                                                              data=dfcu

dfcu_t <- tabular((Regions = factor(en_rgnames)) ~  All(dfcu)*(mean),data = dfcu)
latex(dfcu_t)

tabular( Species ~
           All(iris)*(mean + sd), data=iris )


                                                              


table(df$H01_02)
# Added code while writing paper May 08, 2020


df %>% filter(edu_4 !="Secondary") %>%  group_by(YEAR,edu_4) %>% 
    summarise(mwage=mean(wage*12))



glimpse(df_colleges)
table(df_colleges$OKATO)

temp <- df_colleges %>% filter(OKATO=="Белгородская область")
# Has no bus.gov data

glimpse(df_universities)
table(df_universities$OKATO)


temp <- df_universities %>% filter(OKATO=="Республика Карачаево-Черкесия")
# one uni, does have bus.gov data

glimpse(df_desc)
df_desc

glimpse(df_year_edu_region_age_salary)

glimpse(df_rosstat)

table(df_rosstat$edu_4)
rm(wage_regions)


### Line 764

glimpse(df_year_edu_region_age_salary)
table(df_year_edu_region_age_salary$YEAR)

class(df_year_edu_region_age_salary$H00_02)
levels(df_year_edu_region_age_salary$H00_02)


table(df_region_returns_college_full$Dep_reg)
table(df_region_returns_university_full$Dep_reg)
glimpse(df_region_returns_university_full)

length(col_HE)
col_HE2b

glimpse(arr_name)

class(col_HE2b)
