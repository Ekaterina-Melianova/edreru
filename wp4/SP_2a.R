
# SP_2a.R

# Map of Mincerian returns 

###
library(dplyr)
library(descr)
library(rio) 
library(rgdal)
library(leaflet)
library(sp)
library(sf)
library(RColorBrewer)
library(raster)
library(rgeos)
library(grDevices)
library(ggplot2)
library(ggrepel)
library(viridis)
library(scales)
library(lemon)
library(ggalt)

#########################################################################################


## Now extract names and coefficients from Rsmry_all
# List for one year
a14  <- Rsmry_all$`2014`



# function to extract edu_yrs coefficient
mine <- function(x,i){
a <- x[[i]]
z <- (a[5])
z$coefficients[2]
}


mine(a14,18)

# empty vector for results
ww <- paste0('AA',sprintf("%02d", seq(1,86)))



for(i in 1:27){
  if(j == 28) next   # did not work for some reason
  assign(ww[[i]],mine(a14,i))
}

for(i in 29:52){
    assign(ww[[i]],mine(a14,i))
}


for(i in 54:86){
  assign(ww[[i]],mine(a14,i))
}

AA28 <- NA
AA53 <- NA


df_14 <- c(AA01,AA02,AA03,AA04,AA05,AA06,AA07,AA08,AA09,AA10,
           AA11,AA12,AA13,AA14,AA15,AA16,AA17,AA18,AA19,AA20,
           AA21,AA22,AA23,AA24,AA25,AA26,AA27,AA28,AA29,AA30,
           AA31,AA32,AA33,AA34,AA35,AA36,AA37,AA38,AA39,AA40,
           AA41,AA42,AA43,AA44,AA45,AA46,AA47,AA48,AA49,AA50,
           AA51,AA52,AA53,AA54,AA55,AA56,AA57,AA58,AA59,AA60,
           AA61,AA62,AA63,AA64,AA65,AA66,AA67,AA68,AA69,AA70,
           AA71,AA72,AA73,AA74,AA75,AA76,AA77,AA78,AA79,AA80,
           AA81,AA82,AA83,AA84,AA85,AA86)
res14 <- as.data.frame(cbind(names(a14),df_14))
names(res14) <- c("en_rgnames","mincer")
res14$en_rgnames <- as.character(res14$en_rgnames)
res14$mincer <- as.numeric(as.character(res14$mincer))


###########################################################################################


### Spatial

# Load GADM map of Russia
gadm_russia <- readRDS("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/gadm36_RUS_1_sp.rds")
# Reproject in different coordinates
proj4.str <- CRS("+init=epsg:3413 +lon_0=105")
gadm.prj <- spTransform(gadm_russia, proj4.str)


class(gadm.prj)
# convert to an sf object
rus1.sf <- sf::st_as_sf(gadm.prj,proj4.str)
class(rus1.sf)
#plot(rus1.sf,max.plot=1)



## Load variables to map gadm and returns
rgrs <- rio::import("C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary/rgrs.xlsx")
rgrs2 <- rgrs %>% dplyr::select(GID_1,IC_id)
str(rgrs2)
rgvars2 <- rgvars %>% dplyr::select(OKATO,en_rgnames,IC_ID)
rgvars2$IC_id <- as.character(rgvars2$IC_ID)
rgvars2$IC_ID <- NULL
rgrs3 <- left_join(rgrs2,rgvars2, by="IC_id")

###############################################################


## Now to plot something meaningful
rgrs4 <- left_join(res14,rgrs3,by="en_rgnames")
temp1 <- rgrs4 %>% dplyr::select(GID_1,mincer)
rus1.sf_ <- left_join(rus1.sf,temp1,by="GID_1")
#glimpse(rus1.sf_)


# Scale of 8 colors
library(scales)
show_col(viridis_pal()(8))

library(scales)
q_colors =  8
v_colors =  viridis(q_colors)

range(rus1.sf_$mincer,na.rm = TRUE)
rus1.sf_$cat15 <- cut(rus1.sf_$mincer,breaks=c(-Inf,0.03,0.04,0.05,
                                               0.06,0.07,0.08,0.09,+Inf),labels=c("< 3%","3-4%","4-5%",
                                                                                  "5-6%","6-7%","7-8%","8-9%","> 9%"))
descr::freq(rus1.sf_$cat15)


p1 <- ggplot(data=rus1.sf_) + geom_sf(aes(fill=cat15)) +
  scale_fill_manual(values=as.vector(v_colors),name="Mincerian Returns 2014") +
  #  theme(legend.title=element_text(size=10),
  #        legend.background = element_rect(fill="yellow")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 8),
        legend.key.size = unit(0.5, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))
a <- theme_get()
p1 <- p1 + theme_set(a) + cowplot::draw_label("2014", hjust=0,vjust=2,size=8) +  theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank())
p1


##############################

## Now extract names and coefficients from Rsmry_all
# List for one year
a15  <- Rsmry_all$`2015`

# function to extract edu_yrs coefficient
mine <- function(x,i){
  a <- x[[i]]
  z <- (a[5])
  z$coefficients[2]
}
# empty vector for results
ww <- paste0('AA',sprintf("%02d", seq(1,86)))

for(i in 1:27){
  if(j == 28) next   # did not work for some reason
  assign(ww[[i]],mine(a15,i))
}
for(i in 29:52){
  assign(ww[[i]],mine(a15,i))
}

for(i in 54:86){
  assign(ww[[i]],mine(a15,i))
}
AA28 <- NA
AA53 <- NA
df_15 <- c(AA01,AA02,AA03,AA04,AA05,AA06,AA07,AA08,AA09,AA10,
           AA11,AA12,AA13,AA14,AA15,AA16,AA17,AA18,AA19,AA20,
           AA21,AA22,AA23,AA24,AA25,AA26,AA27,AA28,AA29,AA30,
           AA31,AA32,AA33,AA34,AA35,AA36,AA37,AA38,AA39,AA40,
           AA41,AA42,AA43,AA44,AA45,AA46,AA47,AA48,AA49,AA50,
           AA51,AA52,AA53,AA54,AA55,AA56,AA57,AA58,AA59,AA60,
           AA61,AA62,AA63,AA64,AA65,AA66,AA67,AA68,AA69,AA70,
           AA71,AA72,AA73,AA74,AA75,AA76,AA77,AA78,AA79,AA80,
           AA81,AA82,AA83,AA84,AA85,AA86)
res15 <- as.data.frame(cbind(names(a15),df_15))
names(res15) <- c("en_rgnames","mincer")
res15$en_rgnames <- as.character(res15$en_rgnames)
res15$mincer <- as.numeric(as.character(res15$mincer))
############################################
## Now extract names and coefficients from Rsmry_all
# List for one year
a16  <- Rsmry_all$`2016`

# function to extract edu_yrs coefficient
mine <- function(x,i){
  a <- x[[i]]
  z <- (a[5])
  z$coefficients[2]
}
# empty vector for results
ww <- paste0('AA',sprintf("%02d", seq(1,86)))

for(i in 1:27){
  if(j == 28) next   # did not work for some reason
  assign(ww[[i]],mine(a16,i))
}
for(i in 29:52){
  assign(ww[[i]],mine(a16,i))
}

for(i in 54:86){
  assign(ww[[i]],mine(a16,i))
}
AA28 <- NA
AA53 <- NA
df_16 <- c(AA01,AA02,AA03,AA04,AA05,AA06,AA07,AA08,AA09,AA10,
           AA11,AA12,AA13,AA14,AA15,AA16,AA17,AA18,AA19,AA20,
           AA21,AA22,AA23,AA24,AA25,AA26,AA27,AA28,AA29,AA30,
           AA31,AA32,AA33,AA34,AA35,AA36,AA37,AA38,AA39,AA40,
           AA41,AA42,AA43,AA44,AA45,AA46,AA47,AA48,AA49,AA50,
           AA51,AA52,AA53,AA54,AA55,AA56,AA57,AA58,AA59,AA60,
           AA61,AA62,AA63,AA64,AA65,AA66,AA67,AA68,AA69,AA70,
           AA71,AA72,AA73,AA74,AA75,AA76,AA77,AA78,AA79,AA80,
           AA81,AA82,AA83,AA84,AA85,AA86)
res16 <- as.data.frame(cbind(names(a16),df_16))
names(res16) <- c("en_rgnames","mincer")
res16$en_rgnames <- as.character(res16$en_rgnames)
res16$mincer <- as.numeric(as.character(res16$mincer))
#######################################
## Now extract names and coefficients from Rsmry_all
# List for one year
a17  <- Rsmry_all$`2017`

# function to extract edu_yrs coefficient
mine <- function(x,i){
  a <- x[[i]]
  z <- (a[5])
  z$coefficients[2]
}
# empty vector for results
ww <- paste0('AA',sprintf("%02d", seq(1,86)))

for(i in 1:27){
  if(j == 28) next   # did not work for some reason
  assign(ww[[i]],mine(a17,i))
}
for(i in 29:52){
  assign(ww[[i]],mine(a17,i))
}

for(i in 54:86){
  assign(ww[[i]],mine(a17,i))
}
AA28 <- NA
AA53 <- NA
df_17 <- c(AA01,AA02,AA03,AA04,AA05,AA06,AA07,AA08,AA09,AA10,
           AA11,AA12,AA13,AA14,AA15,AA16,AA17,AA18,AA19,AA20,
           AA21,AA22,AA23,AA24,AA25,AA26,AA27,AA28,AA29,AA30,
           AA31,AA32,AA33,AA34,AA35,AA36,AA37,AA38,AA39,AA40,
           AA41,AA42,AA43,AA44,AA45,AA46,AA47,AA48,AA49,AA50,
           AA51,AA52,AA53,AA54,AA55,AA56,AA57,AA58,AA59,AA60,
           AA61,AA62,AA63,AA64,AA65,AA66,AA67,AA68,AA69,AA70,
           AA71,AA72,AA73,AA74,AA75,AA76,AA77,AA78,AA79,AA80,
           AA81,AA82,AA83,AA84,AA85,AA86)
res17 <- as.data.frame(cbind(names(a17),df_17))
names(res17) <- c("en_rgnames","mincer")
res17$en_rgnames <- as.character(res17$en_rgnames)
res17$mincer <- as.numeric(as.character(res17$mincer))
##############################################
# List for one year
a18  <- Rsmry_all$`2018`

# function to extract edu_yrs coefficient
mine <- function(x,i){
  a <- x[[i]]
  z <- (a[5])
  z$coefficients[2]
}
# empty vector for results
ww <- paste0('AA',sprintf("%02d", seq(1,86)))

for(i in 1:27){
  if(j == 28) next   # did not work for some reason
  assign(ww[[i]],mine(a18,i))
}
for(i in 29:52){
  assign(ww[[i]],mine(a18,i))
}

for(i in 54:86){
  assign(ww[[i]],mine(a18,i))
}
AA28 <- NA
AA53 <- NA
df_18 <- c(AA01,AA02,AA03,AA04,AA05,AA06,AA07,AA08,AA09,AA10,
           AA11,AA12,AA13,AA14,AA15,AA16,AA17,AA18,AA19,AA20,
           AA21,AA22,AA23,AA24,AA25,AA26,AA27,AA28,AA29,AA30,
           AA31,AA32,AA33,AA34,AA35,AA36,AA37,AA38,AA39,AA40,
           AA41,AA42,AA43,AA44,AA45,AA46,AA47,AA48,AA49,AA50,
           AA51,AA52,AA53,AA54,AA55,AA56,AA57,AA58,AA59,AA60,
           AA61,AA62,AA63,AA64,AA65,AA66,AA67,AA68,AA69,AA70,
           AA71,AA72,AA73,AA74,AA75,AA76,AA77,AA78,AA79,AA80,
           AA81,AA82,AA83,AA84,AA85,AA86)
res18 <- as.data.frame(cbind(names(a18),df_18))
names(res18) <- c("en_rgnames","mincer")
res18$en_rgnames <- as.character(res18$en_rgnames)
res18$mincer <- as.numeric(as.character(res18$mincer))
##################
###############################################################
###############################################################
###############################################################
###############################################################
# 2015
rgrs4 <- left_join(res15,rgrs3,by="en_rgnames")
temp1 <- rgrs4 %>% dplyr::select(GID_1,mincer)
rus1.sf_ <- left_join(rus1.sf,temp1,by="GID_1")
#glimpse(rus1.sf_)
# Scale of 8 colors
library(scales)
show_col(viridis_pal()(8))
library(scales)
q_colors =  8
v_colors =  viridis(q_colors)
#range(rus1.sf_$mincer,na.rm = TRUE)
rus1.sf_$cat15 <- cut(rus1.sf_$mincer,breaks=c(-Inf,0.03,0.04,0.05,
                                               0.06,0.07,0.08,0.09,+Inf),labels=c("< 3%","3-4%","4-5%",
                                                                                  "5-6%","6-7%","7-8%","8-9%","> 9%"))
descr::freq(rus1.sf_$cat15)


p2 <- ggplot(data=rus1.sf_) + geom_sf(aes(fill=cat15)) +
  scale_fill_manual(values=as.vector(v_colors),name="Mincerian Returns 2015") +
  #  theme(legend.title=element_text(size=10),
  #        legend.background = element_rect(fill="yellow")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 8),
        legend.key.size = unit(0.5, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))

a <- theme_get()
p2 <- p2 + theme_set(a) + cowplot::draw_label("2015", hjust=0,vjust=2,size=8) +  theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank())
p2
##################################################%%^%^%^^^^&&%^&%&^%^%&^%&^%
# 2016
rgrs4 <- left_join(res16,rgrs3,by="en_rgnames")
temp1 <- rgrs4 %>% dplyr::select(GID_1,mincer)
rus1.sf_ <- left_join(rus1.sf,temp1,by="GID_1")
#glimpse(rus1.sf_)
# Scale of 8 colors
library(scales)
show_col(viridis_pal()(8))

library(scales)
q_colors =  8
v_colors =  viridis(q_colors)

range(rus1.sf_$mincer,na.rm = TRUE)
rus1.sf_$cat15 <- cut(rus1.sf_$mincer,breaks=c(-Inf,0.03,0.04,0.05,
                                               0.06,0.07,0.08,0.09,+Inf),labels=c("< 3%","3-4%","4-5%",
                                                                                  "5-6%","6-7%","7-8%","8-9%","> 9%"))
descr::freq(rus1.sf_$cat15)
               


p3 <- ggplot(data=rus1.sf_) + geom_sf(aes(fill=cat15)) +
  scale_fill_manual(values=as.vector(v_colors),name="Mincerian Returns 2016") +
  #  theme(legend.title=element_text(size=10),
  #        legend.background = element_rect(fill="yellow")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 8),
        legend.key.size = unit(0.5, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))
a <- theme_get()
p3 <- p3 + theme_set(a) + cowplot::draw_label("2016", hjust=0,vjust=2,size=8) +  theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank())
p3
###############################
# 2017
rgrs4 <- left_join(res17,rgrs3,by="en_rgnames")
temp1 <- rgrs4 %>% dplyr::select(GID_1,mincer)
rus1.sf_ <- left_join(rus1.sf,temp1,by="GID_1")
#glimpse(rus1.sf_)
# Scale of 8 colors
library(scales)
show_col(viridis_pal()(8))

library(scales)
q_colors =  8
v_colors =  viridis(q_colors)

range(rus1.sf_$mincer,na.rm = TRUE)
rus1.sf_$cat15 <- cut(rus1.sf_$mincer,breaks=c(-Inf,0.03,0.04,0.05,
                                               0.06,0.07,0.08,0.09,+Inf),labels=c("< 3%","3-4%","4-5%",
                                                                                  "5-6%","6-7%","7-8%","8-9%","> 9%"))
descr::freq(rus1.sf_$cat15)

p4 <- ggplot(data=rus1.sf_) + geom_sf(aes(fill=cat15)) +
  scale_fill_manual(values=as.vector(v_colors),name="Mincerian Returns 2017") +
  #  theme(legend.title=element_text(size=10),
  #        legend.background = element_rect(fill="yellow")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 8),
        legend.key.size = unit(0.5, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))
a <- theme_get()
p4 <- p4 + theme_set(a) + cowplot::draw_label("2017", hjust=0,vjust=2,size=8) +  theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank())
p4
#################################################
# 2018
rgrs4 <- left_join(res18,rgrs3,by="en_rgnames")
temp1 <- rgrs4 %>% dplyr::select(GID_1,mincer)
rus1.sf_ <- left_join(rus1.sf,temp1,by="GID_1")
#glimpse(rus1.sf_)
# Scale of 8 colors
library(scales)
show_col(viridis_pal()(8))

library(scales)
q_colors =  8
v_colors =  viridis(q_colors)

range(rus1.sf_$mincer,na.rm = TRUE)
rus1.sf_$cat15 <- cut(rus1.sf_$mincer,breaks=c(-Inf,0.03,0.04,0.05,
                                               0.06,0.07,0.08,0.09,+Inf),labels=c("< 3%","3-4%","4-5%",
                                                                                  "5-6%","6-7%","7-8%","8-9%","> 9%"))
descr::freq(rus1.sf_$cat15)


p5 <- ggplot(data=rus1.sf_) + geom_sf(aes(fill=cat15)) +
  scale_fill_manual(values=as.vector(v_colors),name="Mincerian Returns 2018") +
  #  theme(legend.title=element_text(size=10),
  #        legend.background = element_rect(fill="yellow")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 8),
        legend.key.size = unit(0.5, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)),
         color = guide_legend(override.aes = list(size = 0.5)))
a <- theme_get()
p5 <- p5 + theme_set(a) + cowplot::draw_label("2017", hjust=0,vjust=2,size=8) +  theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank())
p5
