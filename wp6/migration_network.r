# migration_network.R

library(sp)
library(ggmap)
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(raster)

setwd('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary')

# Read additional information about regions
network_stats <- rio::import('regions_network_stats.xlsx', 'xlsx')
# Read edgelist with coordinates 
coordinates_edgelist <- rio::import('migration_network_coordinates_edgelist.xlsx', 'xlsx')
# Read edgelist in xlsx format
edgelist_df <- rio::import('migration_network_edgelist_df.xlsx', 'xlsx')

# Create map
gadm_russia <- readRDS("gadm36_RUS_1_sp.rds")
map_russia <- spTransform(gadm_russia, CRS("+proj=longlat +datum=WGS84"))  
data.f <- fortify(gadm_russia, region = "NAME_1")


### PROPORTIONAL SCALED, number of people
ggplot(data.f) +
  ggtitle("Migration Network of Graduates") +
  geom_polygon(aes(x = long, y = lat, group = group), colour = "white", fill='lightgrey') +
  xlim(15,190) +
  ylim(40,83) +
  coord_map("azequalarea") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) + 
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + 
  geom_segment(data = coordinates_edgelist, arrow = arrow(length = unit(0.01, "npc")),
               aes(x = out_region_long, xend = in_region_long,
                   y = out_region_lat, yend = in_region_lat),
               size = edgelist_df$graduates_number * 0.001,
               # size = edgelist_df$graduates_number*0.001,
               alpha = 0.1) +
  geom_point(data = network_stats,
             aes(long, lat),
             # color = node_colors,
             size=network_stats$in_degree*0.0025, alpha=0.7)

