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
## Split the df into regional sets

## Create slope variables

df1 <- df_universities %>% transmute(SPUNID=SPUNID,s14_15=salary_2015-salary_2014, s15_16=salary_2016-salary_2015, d2S=s15_16-s14_15,
                                     age=round(graduate_years),salary_2014=salary_2014,salary_2015=salary_2015,salary_2016=salary_2016)


df2 <- df1 %>% mutate(x1=age+1,x2=age+2,x3=age+3,y1=salary_2014,y2=salary_2015,y3=salary_2016)

(x <- c(df2[1,]$x1,df2[1,]$x2,df2[1,]$x3))
(y <- c(df2[1,]$salary_2014,df2[1,]$salary_2015,df2[1,]$salary_2016))

xname <- "x"
yname <- "y"
dat <- data.frame(x,y)
names(dat) <- c(xname,yname)

dat


res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat)) %>% as.vector()
res_

# Form age earnings profile

(x <- seq(df2[1,]$x1,length=10))





df_universities <- df_universities %>% arrange(SPUNID) 

split.df <- split(df_universities, df_universities$OKATO)

for (I in 1:length(split.df)) { 
  assign(paste0("RR",unique(split.df[[I]]$OKATO)),split.df[[I]])}


# create empty list of 79 objects

blix <- as.list(paste0("a",seq(1:79)))


## Generate a vector of OKATO
# Generating a vector with years
(seq_okato <- sort(unique(df_universities$OKATO)))

for(i in seq(length(seq_okato))){
  seq_okato[[i]] <<- split.df[[i]] %>% dplyr::select(SPUNID,salary_2014,salary_2015,salary_2016,graduate_years) %>%
    dplyr::transmute(SPUNID=SPUNID,s14_15=salary_2015-salary_2014, s15_16=salary_2016-salary_2015, d2S=s15_16-s14_15) 
}


for(i in seq(length(seq_okato))){
  assign(seq_okato[[i]],split.df[[i]])
}

seq_okato[[i]]

a <- split.df[[1]]

seq(length(seq_okato))




mine <- function(x){ 
         for(i in 1:79){
        blix[[i]] <<- x %>% dplyr::select(SPUNID,salary_2014,salary_2015,salary_2016,graduate_years) %>%
        dplyr::transmute(SPUNID=SPUNID,s14_15=salary_2015-salary_2014, s15_16=salary_2016-salary_2015, d2S=s15_16-s14_15) 
        print(blix[[i]])
        }
}

mine <- function(x){ 
  for(i in 1:79){
    blix[[i]] <<- x %>% dplyr::select(SPUNID,salary_2014,salary_2015,salary_2016,graduate_years) %>%
      dplyr::transmute(SPUNID=SPUNID,s14_15=salary_2015-salary_2014, s15_16=salary_2016-salary_2015, d2S=s15_16-s14_15) 
    print(blix[[i]])
  }
}





my.list1 <- lapply(ls(pattern="RR.*"), get)

# Now I use sapply as a variant of a for loop
sapply(my.list1,mine)

names(blix) <- paste0(ls(pattern="RR.*"),"N1",sep="")
# I take the blix list and spin off into 25 named
# dataframes
list2env(blix,envir=.GlobalEnv)


x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")






