# rev1.R

# Revised run as of April 2021


Sys.setlocale("LC_CTYPE", "russian")


library(dplyr) # data mungeing 
library(qdap) # easier string manipulation
library(reshape2) # wide to long
library(ggplot2) # graphs

# From wp4_part2.rmd
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities_areas.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_colleges.rda")

# From wp4_part3.rmd
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_colleges.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")


# I need to fix some missing region_codes, derived from bus.gov 
sum(is.na(df_universities$region_code))
# df <- df_universities %>% filter(is.na(region_code))  # shows 6 observations
df_universities$region_code[df_universities$region_name=="Kemerovskaya Oblast"] <- 42
df_universities$region_code[df_universities$region_name=="Nizhegorodskaya Oblast"] <- 52
df_universities$region_code[df_universities$region_name=="Novosibirskaya Oblast"] <- 54
df_universities$region_code[df_universities$region_name=="Udmurtskaya Respublika"] <- 18
df_universities$region_code[df_universities$region_name=="Moscow"] <- 77
# Check after fixing
sum(is.na(df_universities$region_code))
# shows zero

# add region name from spdf_universities  412 universities rather than 375 because 
# 37 eliminated as outliers
temp1 <- spdf_universities$ID_graduateedu
temp2 <- spdf_universities$polygon_index
temp <- cbind(temp1,temp2) %>% as.data.frame()
colnames(temp) <- c("ID_graduateedu","region_id")

# Now merge back into df_universities
df_universities <- left_join(df_universities,temp,by="ID_graduateedu")
# Save formally


blix1 <- gadm.prj$GID_1
blix2 <- gadm.prj$NAME_1
blix3 <- gadm.prj$NL_NAME_1
blix <- cbind(blix1,blix2,blix3) %>% as.data.frame()
blix$region_id <- qdap::genXtract(gadm.prj$GID_1, ".", "_")  %>% as.character()
colnames(blix) <- c("GID_1","NAME_1","NL_NAME_1","region_id")
blix$NL_NAME_1[blix$NAME_1=="Moscow City"] <- "Город Москва"   # Though it is NA in gadm36 data for some reason; Capitalized first letters!

df_universities <- left_join(df_universities,blix,by="region_id")

# Housekeeping
rm(blix,blix1,blix2,blix3)
# I generate meaningful ids for universities 

temp1 <- spdf_universities$ID_graduateedu
temp2 <- spdf_universities$polygon_index
temp <- cbind(temp1,temp2) %>% as.data.frame()
colnames(temp) <- c("ID_graduateedu","region_id")
# rename region_id with leading 0
temp$region_id2 <- sprintf("%02d",as.numeric(temp$region_id))
temp <- temp %>% arrange(region_id2)
df_univ_ids <- temp %>% group_by(region_id) %>% mutate(SPUNID = paste0(region_id2,sprintf("%02d",row_number()))) %>% ungroup()

#save(df_univ_ids,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_univ_ids.rda")

#save("df_univ_ids",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_univ_ids.rda")

temp <- df_univ_ids %>% dplyr::select(-region_id)
# merge back to unis dataframe

df_universities <- left_join(df_universities,temp,by="ID_graduateedu")
# Save formally
save("df_universities",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities.rda")

glimpse(df_universities)
glimpse(df_universities_areas)
glimpse(df_univ_ids)

#######################################################################################################################################
# graduate.edu data shows
range(df_universities$graduate_years) # from 23 to 33 years 

# I need 34 to 64 years mean ages and sd ages, by region
# Get age-wise and region wise earnings profiles from Rosstat
a14 <- df1 <- df_year_edu_region_age_salary_universities %>% filter(YEAR==2014) %>% transmute(OKATO=H00_02,age=H01_02,annual_wage=annual_wage) %>%
  filter(age >= 34)

table(a14$OKATO)


# I need to loop by OKATO and then by age
a14 %>% group_by(OKATO) 
t_ <- a14 %>% dplyr::select(OKATO) %>% distinct() %>% arrange(OKATO) 

a14_k <- paste0("aa",t_$OKATO) %>% as.list()

for (i in seq(length(a14_k))){
  a14_k[[i]] <- a14 %>% filter(OKATO==!!(sprintf(" %02d" ,i))) %>% summarise(mwage=mean(annual_wage,na.rm=T))
  }


a14_k[[1]] <- a14 %>% filter(OKATO=="01") %>% summarise(mwage=mean(annual_wage))

a14_k[2]

a14_k

a14_k <- a14_k %>% as.data.frame()

Rlm_mincer_f = Rlm_mincer_m = Rlm_mincer_all 

# Define indices to simplify loops coding 
seq_year <- unique(df$YEAR)
df$H00_02 <- as.numeric(as.character(df$H00_02))
seq_region <- unique(df$H00_02) %>% sort()

# Running loops over each year and region.
# KVZV variable represents samplig weights.
# print(i) is just to display the loop is working.

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







df1 <- df_year_edu_region_age_salary_universities

a15 <- df1  %>% filter(YEAR==2015)


range(a15$H01_02)


a16 <- df1 <- df_year_edu_region_age_salary_universities %>% filter(YEAR==2016) %>% transmute(OKATO=H00_02,age=H01_02,annual_wage=annual_wage)
a17 <- df1 <- df_year_edu_region_age_salary_universities %>% filter(YEAR==2017) %>% transmute(OKATO=H00_02,age=H01_02,annual_wage=annual_wage)
a18 <- df1 <- df_year_edu_region_age_salary_universities %>% filter(YEAR==2018) %>% transmute(OKATO=H00_02,age=H01_02,annual_wage=annual_wage)






df1 <- df_year_edu_region_age_salary_universities
glimpse(df1)

df2 <- df1 %>% transmute(Year=YEAR,OKATO=H00_02,summer=1,aw=annual_wage,age=H01_02) %>% filter(Year==2014) 

df3 <- df2 %>% group_by( OKATO) %>% summarize(wage=mean(aw)) 

a <- df_universities %>% dplyr::select(OKATO,region_name)  %>% unique()

df3a <- left_join(df3,a,by="OKATO") %>% arrange(desc(wage))


























mean(df_universities$salary_2014)
mean(df_universities$salary_2015)
mean(df_universities$salary_2016)


mean(df_universities_areas$graduates_salary_2014)
mean(df_universities_areas$graduates_salary_2015)
mean(df_universities_areas$graduates_salary_2016)

a <- df_universities_areas %>% dplyr::select(area_name) %>% unique() %>% arrange(area_name)
a1 <- df_universities %>% dplyr::select(specialization_type) %>% unique() %>% arrange()



a <- df_universities_areas %>% dplyr::select(area_name) %>% unique() %>% arrange(area_name)

table(df_universities$OKATO)




universities_region_number$region_id
max(as.numeric(df_universities$region_code),na.rm=T)

write.xlsx(file="blix",df_universities$name)

gadm.prj$GID_1
gadm.prj$NAME_1

glimpse(spdf_universities)

a1 <- spdf_universities$region_code
a2 <- spdf_universities$polygon_index

blix <- as.data.frame(cbind(a1,a2))

temp1 <- data.frame(region_id=1:length(gadm.prj))
temp2 <- data.frame(gadm.prj$GID_1)
temp3 <- data.frame(gadm.prj$NAME_1)

(temp <- cbind(temp1,temp2,temp3))


blix <- spdf_universities %>% dplyr::select(region_code,polygon_index)
colnames(blix) <- c(" region_code"," ")






universities_polygon_indices

universities_polygon_indices <- over(spdf_universities,
                                     SpatialPolygons(gadm_russia@polygons, proj4string=gadm_russia@proj4string),
                                     returnList=F)
spdf_universities$polygon_index <- universities_polygon_indices

# Calculate number of organizations in each region
universities_region_number <- as.data.frame(spdf_universities) %>%
  group_by(polygon_index) %>% count() %>% as.data.frame()

colnames(universities_region_number) <- c("region_id", "count")


df_temp <- data.frame(region_id=1:length(gadm.prj))
df_temp <- df_temp %>% left_join(universities_region_number, by='region_id')
df_temp$count[is.na(df_temp$count)] <- 0
# Add number of organizations
gadm.prj$universities_number <- df_temp$count

