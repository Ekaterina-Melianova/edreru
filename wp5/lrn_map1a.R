
# Creating choropleth maps from regional aggregates. 

# Last updated by Suhas, Monday, April 29, 2019
Sys.setlocale("LC_ALL", "russian")

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


setwd("C:/Country/Russia/Data/ECA/RegionalStat/Generated Data")
# Data created by EM 
rgadm_DO_DOP <- rio::import("rgadm_DO_DOP.xlsx")

# for some reason the GADM sf download gave error messages so I use the sp download
rus1 <- readRDS("C:/Country/Russia/Data/Regional/gadm36_RUS_1_sp.rds")
#rus1 <- readRDS("gadm36_RUS_1_sp.rds")
# results in split map - I need to change coordinate system
proj4.str <- CRS("+init=epsg:3413 +lon_0=105")
rus1.prj <- spTransform(rus1, proj4.str)
#plot(rus1.prj)
# create a color pallette with rainbow layers
# color <- rainbow(length(levels(gadm$regions)))
class(rus1.prj)
# convert to an sf object
rus1.sf <- sf::st_as_sf(rus1.prj,proj4.str)
class(rus1.sf)
#plot(rus1.sf,max.plot=1)

## Now to plot something meaningful

temp1 <- rgadm_DO_DOP %>% dplyr::select(GID_1,Coverage_15_RF,Coverage_16_RF,Coverage_17_RF,Coverage_18_RF)

## add the variable to map data

rus1.sf_ <- left_join(rus1.sf,temp1,by="GID_1")


library(scales)
show_col(viridis_pal()(10))


library(scales)
q_colors =  10
v_colors =  viridis(q_colors)

# 2015
rus1.sf_$cat15 <- cut(rus1.sf_$Coverage_15_RF,breaks=c(-Inf,0.1, 0.2,0.3,0.4,0.5,
                                                       0.6,0.7,0.8,0.9,+Inf),labels=c("< 10%","10-20%","20-30%","30-40%","40-50%",
                                                                                      "50-60%","60-70%","70-80%","80-90%","> 90%"))
descr::freq(rus1.sf_$cat15)

p1 <- ggplot(data=rus1.sf_) + geom_sf(aes(fill=cat15)) +
  scale_fill_manual(values=as.vector(v_colors),name="Extra-curricular\n Activities") +
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
p1 <- p1 + theme_set(a) + cowplot::draw_label("2015", hjust=0,vjust=2,size=8) +  theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank())
p1