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

opar <- par() # base graphics parameters

# For each of Rosstat Years, starting with 2019

myfun1 <- function(arg){
  par(mfrow=c(4,5))
  for (i in seq_along(1:length(OKS))){ # to process each region or OKATO in for loop
    a1 <- arg %>% filter(OKATO==OKS[i]) 
    
# TEMP
    a1 <- z19 %>% filter(OKATO=="01")
      
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
dat3g <- df1[df1$OKATO==OKS[i]]
dat3g <- df1[df1$OKATO=="01",]

for (k in seq_along(1:dim(dat3g)[[1]])){
  dat3gy_ <- dat3g[k,c("aw_2014","aw_2015","aw_2016")]  %>% as.numeric() # 3 salaries for uni
  dat3gx_ <- dat3g[k,c("age")]                                           # graduates age. a numeric value
  vat3x <- c(dat3gx_+1,dat3gx_+2,dat3gx_+3)                              #  age vector to go with salaries
  (dat3x <- c(vat3x,36,45,50,55))                                        # vector of 7 ages
  (dat3y <- c(dat3gy_,dat2$y)    %>% as.numeric())# vector of 7 wages  
  dat3 <- data.frame(dat3x,dat3y)
  names(dat3) <- c("x","y")  # new data with 7 points - 3 from df_universities and 4 from regional data
  print(dat3)
}



dat3y_ <- c(232536,268968,295224)  # example of first university in df_universities
dat3x_ <- c(28,29,30)

(dat3x  <- c(dat3x_,36,45,50,55))
(dat3y <- c(dat3y_,dat2$y))

dat3 <- data.frame(dat3x,dat3y)
names(dat3) <- c("x","y")  # new data with 7 points - 3 from df_universities and 4 from regional data
dat3

##  # again estimate quadratic function
(res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat3)) %>% as.vector())


## again project with hybrid values 
xname <- "x"
yname <- "y"

(x <- seq(0:64))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])

x <- x[20:65]
y <- y[20:65]
(dat4 <- data.frame(x,y))
assign(paste0("OKATO.",year,".",OKS[i]),dat4, envir = .GlobalEnv)

ptitle <- paste0("OKATO.",year,".",OKS[i])

plot(dat4$x,dat4$y,type="l",col="red",lwd=2,main=ptitle)
print(paste0("OKS is now"," __ ",OKS[i]))


}
}

# Need to automate this through lapply
myfun1(z19)
myfun1(z18)
myfun1(z17)
myfun1(z16)
myfun1(z15)
myfun1(z14)

par(opar)

##########################
# Now merge the results with df_universities

zf1_p <- df_universities[df_universities$OKATO=="01",]
zf1_ <- zf1_p[1,]

blix_x <- OKATO.19.01$x
blix_y <- OKATO.19.01$y

t1x_ <- data.frame(t(blix_x))
names(t1x_) <- paste0("X",sprintf("%02d",seq(1:46)))

t1y_ <- data.frame(t(blix_y))
names(t1y_) <- paste0("Y",sprintf("%02d",seq(1:46)))

zf1 <- cbind(zf1_,t1x_,t1y_)
glimpse(zf1)


#slist <- my.list[1:2]


my.list <- lapply(ls(pattern="OKATO.19.*"), get)

for (i in seq_along(1:length(OKS)))

for(j in seq(1:dim(df_universities)[1]))
  
for(i in seq(1:length(OKS))){      
zf1_p <- df_universities[df_universities$OKATO==OKS[[i]],]
unireg <- dim(zf1_p)[[1]]
  for(j in seq(1:unireg)){
zf1_ <- zf1_p[j,]
a <- zf1_$OKATO
dat <- my.list[[i]]

blix_x <- dat$x
blix_y <- dat$y

t1x_ <- data.frame(t(blix_x))
names(t1x_) <- paste0("X",sprintf("%02d",seq(1:46)))

t1y_ <- data.frame(t(blix_y))
names(t1y_) <- paste0("Y",sprintf("%02d",seq(1:46)))

SPUNID <- zf1_$SPUNID

zf1 <- cbind(zf1_,t1x_,t1y_)
assign(paste0("ZF.",SPUNID),zf1)
# The dataframe of 1 university
mylist2 <- lapply(ls(pattern="ZF.*"),get)
# list of dataframes of universities in the ith OKATO

ZZ <- rlist::list.rbind(mylist2)
  }
    }
# House cleaning
alist <- c(ls(pattern="ZF.*"))
rm(list=alist)
rm(alist) # 

glimpse(ZZ)

df_universities2 <- ZZ
save("df_universities2",file="C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities2.rda")
load("C:/Country/Russia/Data/SEASHELL/SEABYTE/edreru/wp4/df_universities2.rda")


#################################################################################################################
###########################

df <- df_universities2 %>% mutate(graduate_years=round(graduate_years))

df2 <- df %>% dplyr::select(SPUNID,graduate_years,146:237)

df2x <- df2[,3:48]
df2y <- df2[,49:94]

df3 <- df2 %>% mutate_at(3:48,function(x){ifelse(df$graduate_years==x,1,0)})

df4 <- df3[,3:48]

df17 <- df %>% dplyr::select(salary_2014,salary_2015,salary_2016)


df5_ <- df4 %>%
  rowwise() %>%
  mutate(gynn = names(.)[which.max(c_across(everything()))]) %>% dplyr::select(47) %>%
  transmute(VG=substr(gynn,2,3)) %>% transmute(Y1_=as.numeric(VG)-3) %>% transmute(Y1=sprintf("%02d",Y1_),
                                                                                   Y2=sprintf("%02d",Y1_+1),
                                                                                   Y3=sprintf("%02d",Y1_+2),
                                                                                   Y4=sprintf("%02d",Y1_+3),
                                                                                   T1=sprintf("%02d",Y1_+4),
                                                                                   T2=sprintf("%02d",Y1_+5),
                                                                                   T3=sprintf("%02d",Y1_+6))
                                                                                   

df5 <- cbind(df2,df5_)

efa_ <- df2 %>% dplyr::select(1,2,49:94)  # original y values are simulated regional values

for(j in seq(1:dim(df5)[1]))
  
for (j in seq(1:1)){
  k1 <- NULL
  a1 <- df5[j,]
  viro1 <- paste0("Y",a1$Y1)
  viro2 <- paste0("Y",a1$Y2)
  viro3 <- paste0("Y",a1$Y3)
  viro4 <- paste0("Y",a1$Y4)
  b1 <- a1 %>% dplyr::select(!!viro1,!!viro2,!!viro3,!!viro4)
  w1 <- df5[j,49:94]  # original y values
  giro1 <- paste0("Y",a1$T1)
  giro2 <- paste0("Y",a1$T2)
  giro3 <- paste0("Y",a1$T3)
 # print(giro1)
  # eliminate 1 denoted by p1
  p1 <- as.numeric(a1$T1)
  p2 <- as.numeric(a1$T2)
  p3 <- as.numeric(a1$T3)
  print(paste0("Value of p1 is " ,p1))
  print(paste0("Value of p2 is " ,p2))
  print(paste0("Value of p3 is " ,p3))
  # eliminate corresponding value from w1
  w1[[p3]] <- NULL
  w1[[p2]] <- NULL
  w1[[p1]] <- NULL
  
  w2 <- a1 %>% dplyr::select(salary_2014,salary_2015,salary_2016)
  
  
  print(w2)
  print(w1)
    names(b1) <- c("DC1","DC2","DC3","DC4")
  k1 <- rbind(k1,b1)
  print(k1)
}

x %!in% table



for (j in seq(1:1)){
  k1 <- NULL
  a1 <- df5[j,]
  viro1 <- paste0("Y",a1$Y1)
  b1 <- a1 %>% dplyr::select(!!viro1)
  e1 <- b1[[1]]
  k1 <- rbind(k1,e1)
  print(k1)
}

  
  


for(i in seq_along(1:1)){
a <- colnames(df3[i,3:48])[max.col(df3[i,3:48],ties.method="first")]
print(a)
}

which.max(df3[1,3:48])






df2 <- df %>% dplyr::select(graduate_years, salary_2014:salary_2016,146:237) %>% mutate(graduate_years=round(graduate_years)) 



paste0("X",round(df[1,]$graduate_years)+1,",","X",round(df[1,]$graduate_years)+2,",","X",round(df[1,]$graduate_years)+3)




(res_ <- coef(lm(y ~ poly(x, 2,raw=TRUE), data = dat3)) %>% as.vector())
m <- summary(lm(R_DEN ~ poly(H01_02, 2,raw=TRUE), data = a1))

(res_ <- as.vector(m$coefficients[,1]))
(sres_ <- as.vector(m$coefficients[,2]))

xname <- "x"
yname <- "y"
y2a <- "y2a"
y2b <- "y2b"

(x <- seq(0:64))
(y <- res_[1]+ x*res_[2] + (x^2)*res_[3])
(y2a <- y+(1.96*sres_[2]))
(y2b <- y-(1.96*sres_[2]))

x <- x[22:65]
y <- y[22:65]
y2a <- y2a[22:65]
y2b <- y2b[22:65]
(dat <- data.frame(x,y,y2a,y2b))

plot(dat$x,dat$y,type="l",col="blue",ylim=c(100000,320000))  
lines(dat$x,dat$y2a,col="red",lty=3)
lines(dat$x,dat$y2b,col="red",lty=3)



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



