# rev5a.R

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
library(FinCal)

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

# add region name from spdf_universities  

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


# Get SPUNID into df_universities_areas
df_universities_areas2 <- left_join(df_universities_areas,df_univ_ids,by="ID_graduateedu")
glimpse(df_universities_areas2)

save("df_universities_areas2",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities_areas2.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities_areas2.rda")

####################################################################################################################
##################################################################################################################################
# Load earnings data from Rosstat


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



# No Filtering by age yet
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


save("df_rosstat",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_rosstat.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_rosstat.rda")
###
# house-cleaning
rm(df_,df_14,df_15,df_16,df_17,df_18,df_19,rst_14,rst_15,rst_16,rst_17,rst_18,rst_19)

###############################################################################################################################

####################################################################################################################


## Generate sub-set of df_universities 

df1 <- df_universities %>% transmute(OKATO=OKATO,SPUNID=SPUNID,age=round(graduate_years),aw_2014=salary_2014*12,
                                                                                         aw_2015=salary_2015*12,
                                                                                         aw_2016=salary_2016*12) %>%
                             arrange(SPUNID)



# Rosstat data by year-wise dataframes
z19 <- df %>% filter(YEAR==2019 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher")  %>% dplyr::rename(OKATO=H00_02)
z18 <- df %>% filter(YEAR==2018 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher")  %>% dplyr::rename(OKATO=H00_02)
z17 <- df %>% filter(YEAR==2017 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher")  %>% dplyr::rename(OKATO=H00_02)
z16 <- df %>% filter(YEAR==2016 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher")  %>% dplyr::rename(OKATO=H00_02)
z15 <- df %>% filter(YEAR==2015 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher")  %>% dplyr::rename(OKATO=H00_02)
z14 <- df %>% filter(YEAR==2014 & H01_02 >= 22 & H01_02 <=65 &edu_4=="Higher")  %>% dplyr::rename(OKATO=H00_02)


# Create an ordered list of OKATO in df_universities
OKS_ <- df1 %>% dplyr::select(OKATO) %>% unique() %>% transmute(OKATO=sprintf("%02s",OKATO)) %>% arrange(OKATO) 

# Turn into a vector from a data.frame
OKS <- OKS_[[1]]
OKS


# For each of Rosstat Years, starting with 2019


myfun1 <- function(arg){
  par(mfrow=c(4,5)) # for graphs in 4 by 5 grid
  for (i in seq_along(1:length(OKS))){ # to process each region or OKATO in for loop
    a1 <- arg %>% dplyr::filter(OKATO==OKS[i]) 
(res_ <- coef(lm(R_DEN ~ poly(H01_02, 2,raw=TRUE), data = a1)) %>% as.vector()) # Generate quadratic coefficients from regional earnings
m <- summary(lm(R_DEN ~ poly(H01_02, 2,raw=TRUE), data = a1)) # for Standard errors for later use
(res_ <- as.vector(m$coefficients[,1]))
(sres_ <- as.vector(m$coefficients[,2]))

# I need this value to label graphs - just the value of the year
year <- paste0("",a1$YEAR[[1]]) %>% substr(3,4)

xname <- "x"             # not sure if this is needed
yname <- "y"
y2a <- "y2a"
y2b <- "y2b"

(x <- seq(0:64))     # simulated age-earnings profile using regional coefficients
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])
(y2a <- y+(1.96*sres_[2]))
(y2b <- y-(1.96*sres_[2]))
                     # actually interested in working age only
x <- x[22:65]
y <- y[22:65] %>% round()
y2a <- y2a[22:65] %>% round()
y2b <- y2b[22:65] %>% round()
(dat <<- data.frame(x,y,y2a,y2b))

#plot(dat$x,dat$y,type="l",col="blue",ylim=c(100000,320000))  
#lines(dat$x,dat$y2a,col="red",lty=3)
#lines(dat$x,dat$y2b,col="red",lty=3)

# Select specific age points - 3 ages to later get averages for 36, 45, 50 and 55 as extrapolation  points for later user

dat2 <- dat %>% filter(x %in% c(35:37,44:46,49:51,54:56)) %>%
                 group_by(grp = rep(row_number(), length.out = n(), each = 3))  %>%
                  summarise_all(mean) %>% round()

####################################################################################### TEST TEST TEST

# Now need to look at the universities within the region

# Open a new loop for universities within region
dat3g <- df1[df1$OKATO==OKS[i],]
for (k in seq_along(1:dim(dat3g)[[1]])){
  dat3gy_ <- dat3g[k,c("aw_2014","aw_2015","aw_2016")]  %>% as.numeric() # 3 salaries for uni
  dat3gx_ <- dat3g[k,c("age")]                                           # graduates age. a numeric value
  vat3x <- c(dat3gx_+1,dat3gx_+2,dat3gx_+3)                              #  age vector to go with salaries
  (dat3x <- c(vat3x,36,45,50,55))                                        # vector of 7 ages
  (dat3y <- c(dat3gy_,dat2$y)    %>% as.numeric())# vector of 7 wages  
  dat3 <- data.frame(dat3x,dat3y)
  names(dat3) <- c("x","y")  # new data with 7 points - 3 from df_universities and 4 from regional data
##  # again estimate quadratic function
(res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat3)) %>% as.vector())

## again project with hybrid values 
(x <- seq(0:64))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3]) 
  
y <- round(y)  

# restrict to adult working life
x <- x[20:65]
y <- y[20:65]

# need SPUNID to tag response
dname <- dat3g$SPUNID[[k]]
z <- rep(dname,46)

# data frame to use 
(dat4 <- data.frame(z,x,y))
names(dat4) <- c("SPUNID","x","y")
#print(dat4)
# title to identify non-concave earnings profiles and drop them
ptitle <- paste0("OK.",year,".",OKS[i],"_",dname)
plot(dat4$x,dat4$y,type="l",col="red",lwd=2,main=ptitle)
# save file from each university
assign(paste0("OK.",year,".",OKS[i],"_",dname),dat4, envir = .GlobalEnv)
}
# Now repeat the above loop inside OKATO by the next OKATO
  }
# Closing curly bracket for the year of Rosstat in the myfun1 function
    } 


myfun1(z19)

myfun1(z18)
myfun1(z17)
myfun1(z16)
myfun1(z15)
myfun1(z14)

# Combine data frames into  a list
my.list <- lapply(ls(pattern="OK.19.*"), get)
# convert list into a dataframe.
#ZZ <- as.data.frame(do.call(rbind, my.list)) 


##########################
# Now merge the results with df_universities

# Test with OKATO of 01 and first university 0201
zf1_p <- df_universities[df_universities$OKATO=="01",]
zf1_ <- zf1_p[1,] # first uni in first OKATO

blix_x <- OK.19.01_0201$x
blix_y <- OK.19.01_0201$y

t1x_ <- data.frame(t(blix_x)) # put the X column data into rows and give it names
names(t1x_) <- paste0("X",sprintf("%02d",seq(1:46)))

t1y_ <- data.frame(t(blix_y)) # put the Y column data into rows and give it names
names(t1y_) <- paste0("Y",sprintf("%02d",seq(1:46)))

zf1 <- cbind(zf1_,t1x_,t1y_)
glimpse(zf1)[,146:237]

# house cleaning
rm(zf1_p,zf1_,zf1,blix_x,blix_y,t1x_,t1y_,zf1)


#############################
#Now try at scale
# SPs function from long to wide

splowide <- function(M){
  SPUNID <- M$SPUNID[1] %>% as.character()
  blix_x <- M$x
  blix_y <- M$y
  t1x_ <- data.frame(t(blix_x)) # put the X column data into rows and give it names
  names(t1x_) <- paste0("X",sprintf("%02d",seq(1:46)))
  t1y_ <- data.frame(t(blix_y)) # put the Y column data into rows and give it names
  names(t1y_) <- paste0("Y",sprintf("%02d",seq(1:46)))
  return(cbind(SPUNID,t1x_,t1y_))
  }
# Create a list by applying function to 466 univ objects created before with
# synthetic earnings profiles 
d <- my.list[[1]]
splowide(d) # a test
# my.list2 <- rlist::list.rbind(my.list)

blix <- lapply(my.list,splowide)
## convert into dataframe
df_universities2_ <- as.data.frame(do.call(rbind, blix)) 
df_universities2 <- left_join(df_universities,df_universities2_,by="SPUNID") %>% arrange(SPUNID)
glimpse(df_universities2)

save("df_universities2",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities2.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities2.rda")


#################################################################################################################
###########################

## Now add a vector of costs for four years to each university line 

df <- df_universities2 %>% mutate(graduate_years=round(graduate_years))

# select the part with X and Y
df2 <- df %>% dplyr::select(SPUNID,graduate_years,146:237) %>% arrange(SPUNID)

df2x <- df2[,3:48]
df2y <- df2[,49:94]

# Now I want to put 1's in the X_ii where ii is the graduate average age and 0 otherwise
df3 <- df2 %>% mutate_at(3:48,function(x){ifelse(df$graduate_years==x,1,0)})
# only x's 
df4 <- df3[,3:48]


df5_ <- df4 %>%
  rowwise() %>%
  mutate(gynn = names(.)[which.max(c_across(everything()))]) %>% dplyr::select(47) %>%
  transmute(VG=substr(gynn,2,3)) %>% transmute(Y1_=as.numeric(VG)-3) %>% transmute(YY1=sprintf("%02d",Y1_),
                                                                                   YY2=sprintf("%02d",Y1_+1),
                                                                                   YY3=sprintf("%02d",Y1_+2),
                                                                                   YY4=sprintf("%02d",Y1_+3))

# Discovered a problem that some YYs were zeros negative ie  before 01 - I will coerce them to 01
df5_$YY1[df5_$YY1<0] <- "01"
df5_$YY2[df5_$YY2<0] <- "02"
df5_$YY3[df5_$YY3<0] <- "03"
df5_$YY4[df5_$YY4<0] <- "04"

df5_$YY1[df5_$YY1=="00"] <- "01"
df5_$YY2[df5_$YY2=="00"] <- "02"
df5_$YY3[df5_$YY3=="00"] <- "03"
df5_$YY4[df5_$YY4=="00"] <- "04"

# Make sure logic is followed
df5_$YY4[df5_$YY1=="01"] <- "04"
df5_$YY3[df5_$YY1=="01"] <- "03"
df5_$YY2[df5_$YY1=="01"] <- "02"


table(df5_$YY1)
table(df5_$YY2)
table(df5_$YY3)
table(df5_$YY4)

# Now no more 00s or negative

## df5_ is a set of four numbers, like 04 05 06 07  or 06 07 08 09
## which tells me which years I should use to calculate indirect costs
                                                                       
df5 <- cbind(df2,df5_) # I need this reconstruction to pluck the y values

#efa_ <- df2 %>% dplyr::select(1,2,49:94)  # original y values are simulated regional values


# They say don't grow R data frames with a loop in this way, but I 
# can't figure out yet how to do it more efficiently


# First I make an empty data frame I will later populate
# Generate such frames outside the loop if you want to populate them inside !!!
df6 <- data.frame(matrix(ncol = 4, nrow = 466))
colnames(df6) <- c("R1","R2","R3","R4") # Indirect cost and replace which Y values

for (j in seq(1:dim(df5)[1])){
  a1 <- df5[j,]          # select 1st row
  viro1 <- paste0("Y",a1$YY1)  # define the Y value when target graduate was in school year 1
  viro2 <- paste0("Y",a1$YY2)  # define the Y value when target graduate was in school year 2
  viro3 <- paste0("Y",a1$YY3)  # define the Y value when target graduate was in school year 3
  viro4 <- paste0("Y",a1$YY4)  # define the Y value when target graduate was in school year 4
  # I evaluate the value of the viros that were just names before !! stands for evaluate
  b1 <- a1 %>% dplyr::select(!!viro1,!!viro2,!!viro3,!!viro4) 
   # print(b1) # just to check that the loop is doing its work
  z1 <- names(b1)
  df6[j,1:4] <- z1
  print(z1)
  }

# Now I add back to the df_universities

df_universities3 <- cbind(df_universities2,df6) %>% arrange(SPUNID)

save("df_universities3",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities3.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities3.rda")


###################
# Now for direct private and social costs



# 1. Universities
df_universities3$paidServices_mean <- df_universities3 %>%
  dplyr::select(cashReceipts_paidServices_2012, cashReceipts_paidServices_2013, cashReceipts_paidServices_2014,
                cashReceipts_paidServices_2015, cashReceipts_paidServices_2016, cashReceipts_paidServices_2017) %>%
  rowMeans(na.rm = T, dims = 1)
# Calculate social cost per graduate
df_universities3$private_cost <- -round(df_universities3$paidServices_mean / (df_universities3$graduates_number*4))


df_universities3$income_total_mean <- df_universities3 %>%
  dplyr::select(income_total_2012, income_total_2013, income_total_2014,
                income_total_2015, income_total_2016, income_total_2017) %>%
  rowMeans(na.rm = T, dims = 1)
# Calculate social cost per graduate
df_universities3$fiscal_cost <- -round(df_universities3$income_total_mean / (df_universities3$graduates_number*4))


## Now for the series of cash flows

## First task is to replace the years when in grad school with cost figures

blix <- df_universities3 %>% 
  dplyr::select(SPUNID,private_cost,fiscal_cost,starts_with("X"),starts_with("Y"),R1:R4) %>% arrange(SPUNID)
glimpse(blix)

sum(is.na(blix$private_cost))  # 13 have NAs
sum(is.na(blix$fiscal_cost))   # 7 have NAs

blix <- blix %>% filter_all(all_vars(!is.na(.))) %>% arrange(SPUNID)

# 20 lost

# As the usual approach, try with one uni
blix[1,]

for (i in seq_along(1:length(blix))){
  af <- blix[i,]
  # Now value of R1 represents first year in college or starting point of series
  s1_ <- af$R1
  sx1_ <- substr(s1_,2,3)
  nsx1_ <- as.numeric(sx1_)
 serx_ <- paste0("X",sprintf("%02d",c(nsx1_:46)))
 incy_ <- paste0("Y",sprintf("%02d",c((nsx1_+4):46)))
 ydf <- af %>% dplyr::select(one_of(incy_)) %>% as.numeric()
 kirrin_p <- c(af$private_cost,ydf)
 kirrin_f <- c(af$fiscal_cost,ydf)
 irr_p <- FinCal::irr(kirrin_p)
 irr_f <- FinCal::irr(kirrin_f)
 print(irr_p)
 print(irr_f)
}


# Remove outliers from IRR

removeOutliers <- function(vec, q=0.01){
  
  # Calculate low and high threshold for outliers based on quantile value q
  low_q <- quantile(vec, q, na.rm=T)
  high_q <- quantile(vec, 1 - q, na.rm=T)
  # Overwrite outliers with NA's
  vec[vec > high_q] <- NA
  vec[vec < low_q] <- NA
  
  return(vec)
}

# Remove outliers in returns
df_colleges$private_returns <- removeOutliers(df_colleges$private_returns)
df_colleges$social_returns <- removeOutliers(df_colleges$social_returns)
df_universities$private_returns <- removeOutliers(df_universities$private_returns)
df_universities$social_returns <- removeOutliers(df_universities$social_returns)


# Remove organizations with NA in returns
df_colleges <- df_colleges[!is.na(df_colleges$private_returns),]
df_colleges <- df_colleges[!is.na(df_colleges$social_returns),]
df_universities <- df_universities[!is.na(df_universities$private_returns),]
df_universities <- df_universities[!is.na(df_universities$social_returns),]


#  # Calculate ranks of private and social returns
#  df_colleges$private_returns_rank <- rank(-df_colleges$private_returns, na.last = "keep")
#  df_colleges$social_returns_rank <- rank(-df_colleges$social_returns, na.last = "keep")
#  df_universities$private_returns_rank <- rank(-df_universities$private_returns, na.last = "keep")
#  df_universities$social_returns_rank <- rank(-df_universities$social_returns, na.last = "keep")
#  
#  # Remove organizations with NA in rank
#  df_colleges <- df_colleges[!is.na(df_colleges$private_returns_rank),]
#  df_colleges <- df_colleges[!is.na(df_colleges$social_returns_rank),]
#  df_universities <- df_universities[!is.na(df_universities$private_returns_rank),]
#  df_universitie

# Create kable tables with top-10/bottom-10 organizations by social returns.
select_top_bottom_n <- 10

### 1. Colleges
df_colleges_table <- df_colleges %>%
  dplyr::select(social_returns, private_returns, name, region_name, income_total_mean, paidServices_mean,
                graduates_number,
                graduate_salary_2014, graduate_salary_2015, graduate_salary_2016) %>%
  arrange(-social_returns)
df_colleges_table <- df_colleges_table[c(1:select_top_bottom_n,
                                         (nrow(df_colleges_table)-select_top_bottom_n+1):nrow(df_colleges_table)),]

### 2. Universities
df_universities_table <- df_universities %>%
  dplyr::select(social_returns, private_returns, 
                name, region_name, income_total_mean, paidServices_mean,
                graduates_number,
                salary_2014, salary_2015, salary_2016) %>%
  arrange(-social_returns)
df_universities_table <- df_universities_table[c(1:select_top_bottom_n,
                                                 (nrow(df_universities_table)-select_top_bottom_n+1):nrow(df_universities_table)),]



