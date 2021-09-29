# rev3.R

# Borrowed from Artёm's script for WP4 in 2020

options("rgdal_show_exportToProj4_warnings"="none") #to suppress warnings
library(tidyverse)
library(sp)
library(maptools)
library(ggmap)
library(raster)
library(rio)
library(sf)
library(dplyr)
library(purrr)
library(raster)
library(osmdata)
library(data.table)
library(rio)
library(ggplot2)
library(ggmap)
library(tmap)
library(rgeos)
library(geosphere)
library(tidyr)
library(osmdata)
library(OpenStreetMap)
library(maps)
library(RColorBrewer)
library(spatstat)
library(igraph)
library(janitor)
require(gridExtra)
library(spatialEco)
library(RJSONIO)
library(RCurl)
library(jsonlite)
library(rangeMapper)
library(geojsonio)
library(descr)
library(BaylorEdPsych) # install from archive as of Apr 4, 2021
library(mvnmle)
library(mice)
library(Amelia)
library(foreign)

Sys.setlocale("LC_CTYPE", "russian")

######################################################################################################
# Load dataframes
df_colleges <- rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/dataframe_colleges.xlsx")
df_universities <- rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/dataframe_universities.xlsx")
df_universities_areas <- rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/dataframe_universities_areas.xlsx")

# Eliminate pesky "New names column added" 
df_colleges <- df_colleges[,2:114]    # n=2317 
df_universities <- df_universities[,2:140] #n=466
df_universities_areas <- df_universities_areas[,2:13] # n=4817

### Recode rare specialization types in universities and colleges
df_colleges$speciality_type[df_colleges$speciality_type == "спортивный"] <- "общий"
df_universities$specialization_type[df_universities$specialization_type == "спортивный"] <- "специализированный"

## Fix some missing OKATO codes
df_colleges$OKATO[df_colleges$OKATO=="Белгородская область"] <- 14
df_universities$OKATO[df_universities$OKATO=="Белгородская область"] <- 14
df_universities$OKATO[df_universities$OKATO=="Республика Карачаево-Черкесия"] <- 91

### Spatial
####################################################################################################

# Load GADM map of Russia
gadm_russia <- readRDS("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/gadm36_RUS_1_sp.rds")

# Create spatial points dataframe for the colleges
spdf_colleges <- SpatialPointsDataFrame(data=df_colleges,
                                        coords=as.matrix(df_colleges[,c("coordinates_long", "coordinates_lat")]),
                                        proj4string=gadm_russia@proj4string)
# Create spatial points dataframe for the universities
spdf_universities <- SpatialPointsDataFrame(data=df_universities,
                                            coords=as.matrix(df_universities[,c("coordinates_long", "coordinates_lat")]),
                                            proj4string=gadm_russia@proj4string)

# Reproject in different coordinates
proj4.str <- CRS("+init=epsg:3413 +lon_0=105")
gadm.prj <- spTransform(gadm_russia, proj4.str)
spdf_colleges.prj <- spTransform(spdf_colleges, proj4.str)
spdf_universities.prj <- spTransform(spdf_universities, proj4.str)

## Colleges
# Get the polygon id for each organization
colleges_polygon_indices <- over(spdf_colleges,
                                 SpatialPolygons(gadm_russia@polygons, proj4string=gadm_russia@proj4string),
                                 returnList=F)
spdf_colleges$polygon_index <- colleges_polygon_indices
# Calculate number of organizations in each region
colleges_region_number <- as.data.frame(spdf_colleges) %>%
  group_by(polygon_index) %>% count() %>% as.data.frame()
colnames(colleges_region_number) <- c("region_id", "count")
df_temp <- data.frame(region_id=1:length(gadm.prj))
df_temp <- df_temp %>% left_join(colleges_region_number, by='region_id')
df_temp$count[is.na(df_temp$count)] <- 0
# Add number of organizations
gadm.prj$colleges_number <- df_temp$count

## Universities
# Get the polygon id for each organization
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

# Save spatial points data frame for later use
save("spdf_colleges",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/spdf_colleges.rda")
save("spdf_universities",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/spdf_universities.rda")

############################################################################################################################

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
############################################################################################################################

# I need to fix some missing region_codes, derived from bus.gov 
sum(is.na(df_colleges$region_code))
# df <- df_colleges %>% filter(is.na(region_code))  # shows 6 observations
df_colleges$region_code[df_colleges$region_name=="Kemerovskaya Oblast"] <- 42
df_colleges$region_code[df_colleges$region_name=="Nizhegorodskaya Oblast"] <- 52
df_colleges$region_code[df_colleges$region_name=="Novosibirskaya Oblast"] <- 54
df_colleges$region_code[df_colleges$region_name=="Udmurtskaya Respublika"] <- 18
df_colleges$region_code[df_colleges$region_name=="Moscow"] <- 77
# Check after fixing
sum(is.na(df_colleges$region_code))
# shows zero

# add region name from spdf_colleges  412 colleges rather than 375 because 
# 37 eliminated as outliers
temp1 <- spdf_colleges$ID_graduateedu
temp2 <- spdf_colleges$polygon_index
temp <- cbind(temp1,temp2) %>% as.data.frame()
colnames(temp) <- c("ID_graduateedu","region_id")

# Now merge back into df_colleges
df_colleges <- left_join(df_colleges,temp,by="ID_graduateedu")
# Save formally


blix1 <- gadm.prj$GID_1
blix2 <- gadm.prj$NAME_1
blix3 <- gadm.prj$NL_NAME_1
blix <- cbind(blix1,blix2,blix3) %>% as.data.frame()
blix$region_id <- qdap::genXtract(gadm.prj$GID_1, ".", "_")  %>% as.character()
colnames(blix) <- c("GID_1","NAME_1","NL_NAME_1","region_id")
blix$NL_NAME_1[blix$NAME_1=="Moscow City"] <- "Город Москва"   # Though it is NA in gadm36 data for some reason; Capitalized first letters!

df_colleges <- left_join(df_colleges,blix,by="region_id")

# Housekeeping
rm(blix,blix1,blix2,blix3)
# I generate meaningful ids for colleges 

temp1 <- spdf_colleges$ID_graduateedu
temp2 <- spdf_colleges$polygon_index
temp <- cbind(temp1,temp2) %>% as.data.frame()
colnames(temp) <- c("ID_graduateedu","region_id")
# rename region_id with leading 0
temp$region_id2 <- sprintf("%02d",as.numeric(temp$region_id))
temp <- temp %>% arrange(region_id2)
df_college_ids <- temp %>% group_by(region_id) %>% mutate(SPCOLID = paste0(region_id2,sprintf("%02d",row_number()))) %>% ungroup()

#save(df_univ_ids,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_univ_ids.rda")

#save("df_univ_ids",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_univ_ids.rda")

temp <- df_college_ids %>% dplyr::select(-region_id)
# merge back to unis dataframe

df_colleges <- left_join(df_colleges,temp,by="ID_graduateedu")
# Save formally
save("df_colleges",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_colleges.rda")

load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_colleges.rda")
####################################################################################################################
##################################################################################################################################
# Also from Rosstat


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
#df_year_edu_region_age_salary_colleges <- df_year_edu_region_age_salary_colleges %>% filter(H00_02 %in% df_colleges_mean_cost$OKATO)
#df_year_edu_region_age_salary_universities <- df_year_edu_region_age_salary_universities %>% filter(H00_02 %in% df_universities_mean_cost$OKATO)


save(df_year_edu_region_age_salary_colleges, file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_colleges.rda")
save(df_year_edu_region_age_salary_universities,file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")

load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_year_edu_region_age_salary_universities.rda")

###############################################################################################################################

####################################################################################################################
## Look at First university in the list, from OKATO 01

## Generate 

df1 <- df_universities %>% transmute(SPUNID=SPUNID,s14_15=salary_2015-salary_2014, s15_16=salary_2016-salary_2015,s14_16=salary_2016-salary_2014,
                                     d2S=s15_16-s14_15,
                                     age=round(graduate_years),salary_2014=salary_2014,salary_2015=salary_2015,salary_2016=salary_2016) %>%
                             arrange(SPUNID)



for (i in seq_along(1:dim(df1)[1])){
df2 <- df1 %>% mutate(x1=age+1,x2=age+2,x3=age+3,y1=salary_2014*12,y2=salary_2015*12,y3=salary_2016*12) 
df2[i,]
(x <- c(df2[i,]$x1,df2[i,]$x2,df2[i,]$x3))
(y <- c(df2[i,]$y1,df2[i,]$y2,df2[i,]$y3))
xname <- "x"
yname <- "y"
dat <- data.frame(x,y)
names(dat) <- c(xname,yname)
dat
(res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat)) %>% as.vector())
# Form age earnings profile
(x <- seq(df2[i,]$x1,length=66-df2[i,]$x1))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])
dat <- data.frame(x,y)
names(dat) <- c(xname,yname)
SPUNID <- rep(df2[i,]$SPUNID,66-df2[i,]$x1)
(dat <- cbind(SPUNID,dat))
assign(paste0("UNI",SPUNID[1]),dat)
}


my.list <- lapply(ls(pattern="UNI*"), get)

a <- as.data.frame(do.call(rbind, my.list[-1])) 

a_ <- a %>% group_by(SPUNID) %>% summarise(miny=min(y))

a2 <- a_ %>% mutate(can=ifelse(miny>=0,1,0)) 

sum(a2$can)  # only 128 without negative values



#########*&(*^&(*^(* )))
#########*
#########*
#########*
df2 <- df1 %>% mutate(x1=age+1,x2=age+2,x3=age+3,y1=salary_2014*12,y2=salary_2015*12,y3=salary_2016*12) 
df2[1,]
(x <- c(df2[1,]$x1,df2[1,]$x2,df2[1,]$x3))
(y <- c(df2[1,]$y1,df2[1,]$y2,df2[1,]$y3))
xname <- "x"
yname <- "y"
dat <- data.frame(x,y)
names(dat) <- c(xname,yname)
dat
(res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat)) %>% as.vector())
# Form age earnings profile
(x <- seq(df2[1,]$x1,length=66-df2[1,]$x1))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])

(x <- seq(22,length=66-df2[1,]$x1))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])

dat <- data.frame(x,y)
names(dat) <- c(xname,yname)
plot(dat$x,dat$y)

SPUNID <- rep(df2[1,]$SPUNID,66-df2[1,]$x1)
(dat <- cbind(SPUNID,dat))

dat %>% tibble::rownames_to_column() %>% mutate(ismax = (y==max(y)), tgtrow=as.numeric(ifelse(ismax,round(x),0)),
                                                sup=(ifelse(ismax,round(x+5),0)),
                                                inf=(ifelse(ismax,round(x-5),0))) %>% 
                                           filter(round(x) <= max(sup) &  round(x) >= max(inf))
                                                                                                                     


 #  
     
 

 # https://stackoverflow.com/questions/29273012/find-first-occurence-of-value-in-group-using-dplyr-mutate 
 
 
 
z1 <- df_year_edu_region_age_salary_universities 
(z14 <- z1 %>% filter(H00_02=="01" & YEAR==2014) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z15 <- z1 %>% filter(H00_02=="01" & YEAR==2015) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z16 <- z1 %>% filter(H00_02=="01" & YEAR==2016) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z17 <- z1 %>% filter(H00_02=="01" & YEAR==2017) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z18 <- z1 %>% filter(H00_02=="01" & YEAR==2018) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))
(z19 <- z1 %>% filter(H00_02=="01" & YEAR==2019) %>% mutate(ismaxR=(annual_wage==max(annual_wage))) %>% filter(ismaxR))


seq_along(1:5)


z2 <- z1 %>% group_by(H00_02,H01_02) %>% summarize(aws=mean(annual_wage))

(z2_01 <- z2 %>% filter(H00_02=="01") %>% mutate(ismax=(aws==max(aws))) %>% filter(ismax))

# vector of regions in data
(regs <- z2 %>% dplyr::select(H00_02) %>% unique() %>% arrange())

regs[1]

z2 %>% dplyr::select(H00_02) %>% unique()

result <- vector("list",85)

for (i in regs[1]){
blix <<- z2  %>% group_by(H00_02) %>% mutate(blix=(aws==max(aws))) %>% filter(blix)
}


z3 <- z2 %>% group_by(H01_02) %>% summarise(aaws=mean(aws))
plot(z3$H01_02,z3$aaws)

plot(z1[z1$H00_02=="01",]$H01_02, z1[z1$H00_02=="01",]$annual_wage)


ggplot(data=df[df$H00_02=="01",],aes(x=H01_02,y=wage)) +
  geom_smooth()



var <- enquo(var)
col=!!var

zf1 <- df_year_edu_region_age_salary_universities %>% dplyr::rename(OKATO=H00_02)

zf2 <- zf1 %>% filter(OKATO=="01")

glimpse(zf1)






x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")


table(df_universities$graduate_years)



