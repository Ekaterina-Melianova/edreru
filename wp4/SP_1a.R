#SP_1a.R

# Table of regional returns

library(dplyr)
library(openxlsx)

glimpse(df_region_returns_university_full)


mrs <- read.xlsx("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp3/RoREs_cleaned.xlsx") %>%
        dplyr::select(OKATO)


dfc_ <- left_join(df_region_returns_college ,mrs,by=c("region"="OKATO"))
glimpse(dfc_)

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
latex.tabular(dfcu_t)


library(openxlsx)

# line 1218 in wp5_mainSP.Rmd of May 13, 2020

df_colleges_table

write.xlsx(df_colleges_table,file="blix.xlsx",asTable = TRUE)


df_universities_table <- df_universities %>%
  dplyr::select(social_returns, private_returns, 
                name, region_name, region_code, income_total_mean, paidServices_mean,
                graduates_number,
                salary_2014, salary_2015, salary_2016) %>%
  arrange(region_code,name) %>% 
  
  transmute(social_returns=round(social_returns,4),
            private_returns=round(private_returns,4),
            name=name,
            region_name=region_name,
            tot_revenue=round((income_total_mean/1000000),2),
            tot_pdfees=round((paidServices_mean/1000000),2),
            graduates=graduates_number,
            sal14=round(salary_2014/1000),
            sal15=round(salary_2015/1000),
            sal16=round(salary_2016/1000))

write.xlsx(df_universities_table,file="WP5_Universities Social Private Returns.xlsx",asTable = TRUE)


#tabular( Species ~ All(iris)*(mean + sd), data=iris )


                                                              


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

glimpse(df_universities)


temp <- df_colleges %>% distinct(name)

glimpse(df_colleges)

table(df_universities$specialization_type)

##

glimpse(df_universities)
table(df_universities$specialization_type)

##

