library(tidyverse)
library(here)
library(plyr)
library(data.table)
library(scales)
library(RColorBrewer) 
library(sf)
library(tmap)
library(RColorBrewer)
library(cowplot)

##### plot 1 ####
path <- 'E:\\Chapter2\\results'
file_name = paste(path, 'all_segmax.csv',sep = '/')
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

nc <- all_segfin %>% filter(location == 'nocontrol')
all_segfin <- all_segfin %>%  filter(p == 1 & rem == 1)

all_segfin <- rbind(all_segfin, nc)
all_segfin <- all_segfin %>% select(segment, count, location)
colnames(all_segfin)[1] <- "Segment"
colnames(all_segfin)[2] <- "Crayfish abundance"

all_segfin$location[all_segfin$location == 'nocontrol'] <- 1
all_segfin$location[all_segfin$location == 'abund'] <- 2
all_segfin$location[all_segfin$location == 'grow'] <- 3
all_segfin$location[all_segfin$location == 'edge'] <- 4
all_segfin$location[all_segfin$location == 'down'] <- 5
all_segfin$location[all_segfin$location == 'random'] <- 6


all_segfin$location <- as.numeric(all_segfin$location)

level_order <- c("No removal", "Abundance, 1", "Growth, 1","Edges, 1","Downstream, 1",   "Random, 1")

jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))
jdr_abund <- merge(jdr_20k_sf, all_segfin, by  = "Segment")


breaks = c(0.00001, 1, 2, 4, 6, 8, 35) * 100000
colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )
color <- RColorBrewer::brewer.pal(11, "Paired")[1]


bg <- basemaps::basemap_raster(ext=jdr_abund, map_service = "esri", map_type = "world_hillshade")

tm_shape(bg) +  
  tm_rgb()+
  tm_shape(jdr_abund) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 5
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.labels = level_order,
            panel.label.size = 1,
            legend.text.size = 0.8, 
            legend.width = 15, 
            frame = "black",
            legend.show=FALSE,
            frame.lwd = 0,
            panel.label.bg.color = NA)


## no removal sub
g <- basemaps::basemap_raster(ext=jdr_abund, map_service = "esri", map_type = "world_hillshade")

jdr_abundnc <- jdr_abund %>% filter(location == 1)

tm_shape(bg) +  
  tm_rgb()+
  tm_shape(jdr_abundnc) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 8
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.labels = level_order,
            panel.label.size = 1,
            legend.text.size = 0.8, 
            legend.width = 15, 
            frame = "black",
            #legend.show=FALSE,
            frame.lwd = 0,
            panel.label.bg.color = NA)

##### plot 4 ####
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

nc <- all_segfin %>% filter(location == 'nocontrol')
all_segfin <- all_segfin %>%  filter(p == 1 & rem == 4)

all_segfin <- rbind(all_segfin) #, nc)
all_segfin <- all_segfin %>% select(segment, count, location)
colnames(all_segfin)[1] <- "Segment"
colnames(all_segfin)[2] <- "Crayfish abundance"

#all_segfin$location[all_segfin$location == 'nocontrol'] <- 1
all_segfin$location[all_segfin$location == 'abund'] <- 2
all_segfin$location[all_segfin$location == 'grow'] <- 3
all_segfin$location[all_segfin$location == 'edge'] <- 4
all_segfin$location[all_segfin$location == 'down'] <- 5
all_segfin$location[all_segfin$location == 'random'] <- 6


all_segfin$location <- as.numeric(all_segfin$location)

level_order <- c("Abundance, 4", "Growth, 4","Edges, 4", "Downstream, 4",  "Random, 4")

jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))
jdr_abund <- merge(jdr_20k_sf, all_segfin, by  = "Segment")


breaks = c(0.00001, 1, 2, 4, 6, 8, 10) * 100000
colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )
color <- RColorBrewer::brewer.pal(11, "Paired")[1]

tm_shape(bg) +  
  tm_rgb()+
  tm_shape(jdr_abund) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 5
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.labels = level_order,
            panel.label.size = 1,
            legend.text.size = 0.8, 
            legend.width = 15, 
            frame = "black",
            legend.show=TRUE,
            frame.lwd = 0,
            panel.label.bg.color = NA)


##### plot 8 ####
path <- 'E:\\Chapter2\\results'
file_name = paste(path, 'all_segmax.csv',sep = '/')
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

nc <- all_segfin %>% filter(location == 'nocontrol')
all_segfin <- all_segfin %>%  filter(p == 1 & rem == 8)

all_segfin <- rbind(all_segfin) #, nc)
all_segfin <- all_segfin %>% select(segment, count, location)
colnames(all_segfin)[1] <- "Segment"
colnames(all_segfin)[2] <- "Crayfish abundance"

#all_segfin$location[all_segfin$location == 'nocontrol'] <- 1
all_segfin$location[all_segfin$location == 'abund'] <- 2
all_segfin$location[all_segfin$location == 'grow'] <- 3
all_segfin$location[all_segfin$location == 'edge'] <- 4
all_segfin$location[all_segfin$location == 'down'] <- 5
all_segfin$location[all_segfin$location == 'random'] <- 6


all_segfin$location <- as.numeric(all_segfin$location)

level_order <- c("Abundance, 8", "Growth, 8","Edges, 8","Downstream, 8","Random, 8")

jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))
jdr_abund <- merge(jdr_20k_sf, all_segfin, by  = "Segment")


breaks = c(0.00001, 1, 2, 4, 6, 8, 10) * 100000
colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )
color <- RColorBrewer::brewer.pal(11, "Paired")[1]


tm_shape(bg) +  
  tm_rgb()+
  tm_shape(jdr_abund) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 5
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.labels = level_order,
            panel.label.size = 1,
            legend.text.size = 0.8, 
            legend.width = 15, 
            frame = "black",
            legend.show=FALSE,
            frame.lwd = 0,
            panel.label.bg.color = NA)

##### plot 16 ####
path <- 'E:\\Chapter2\\results'
file_name = paste(path, 'all_segmax.csv',sep = '/')
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

nc <- all_segfin %>% filter(location == 'nocontrol')
all_segfin <- all_segfin %>%  filter(p == 1 & rem == 16)

all_segfin <- rbind(all_segfin, nc)
all_segfin <- all_segfin %>% select(segment, count, location)
colnames(all_segfin)[1] <- "Segment"
colnames(all_segfin)[2] <- "Crayfish abundance"

all_segfin$location[all_segfin$location == 'nocontrol'] <- 1
all_segfin$location[all_segfin$location == 'abund'] <- 2
all_segfin$location[all_segfin$location == 'grow'] <- 3
all_segfin$location[all_segfin$location == 'edge'] <- 4
all_segfin$location[all_segfin$location == 'down'] <- 5
all_segfin$location[all_segfin$location == 'random'] <- 6


all_segfin$location <- as.numeric(all_segfin$location)

level_order <- c("No removal", "Target Abundance", "Target Growth", "Target Edges","Target Downstream", "Target Random")

jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))
jdr_abund <- merge(jdr_20k_sf, all_segfin, by  = "Segment")


breaks = c(700000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000) 
colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )
color <- RColorBrewer::brewer.pal(11, "Paired")[1]

bg <- basemaps::basemap_raster(ext=jdr_abund, map_service = "esri", map_type = "world_hillshade")


#tmap_options(bg.color = 'white', legend.text.color = 'black') +
tm_shape(bg) +  
  tm_rgb()+
tm_shape(jdr_abund) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 5
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.labels = level_order,
            panel.label.size = 1,
            legend.text.size = 0.8, 
            legend.width = 15, 
            frame = "black",
            legend.show=TRUE,
            frame.lwd = 0,
            panel.label.bg.color = NA)

##### plot 1 again ####
path <- 'E:\\Chapter2\\results'
file_name = paste(path, 'all_segmax.csv',sep = '/')
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

nc <- all_segfin %>% filter(location == 'nocontrol')
all_segfin <- all_segfin %>%  filter(p == 1 & rem == 1)

all_segfin <- rbind(all_segfin, nc)
all_segfin <- all_segfin %>% select(segment, count, location)
colnames(all_segfin)[1] <- "Segment"
colnames(all_segfin)[2] <- "Crayfish abundance"

all_segfin$location[all_segfin$location == 'nocontrol'] <- 1
all_segfin$location[all_segfin$location == 'abund'] <- 2
all_segfin$location[all_segfin$location == 'grow'] <- 3
all_segfin$location[all_segfin$location == 'edge'] <- 4
all_segfin$location[all_segfin$location == 'down'] <- 5
all_segfin$location[all_segfin$location == 'random'] <- 6


all_segfin$location <- as.numeric(all_segfin$location)

level_order <- c("No removal", "Target Abundance", "Target Growth", "Target Edges","Target Downstream", "Target Random")

jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))
jdr_abund <- merge(jdr_20k_sf, all_segfin, by  = "Segment")


breaks = c(0.00001, 1, 2, 4, 6, 8, 10) * 100000
colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )
color <- RColorBrewer::brewer.pal(11, "Paired")[1]

bg <- basemaps::basemap_raster(ext=jdr_abund, map_service = "esri", map_type = "world_hillshade")


#tmap_options(bg.color = 'white', legend.text.color = 'black') +
tm_shape(bg) +  
  tm_rgb()+
  tm_shape(jdr_abund) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 5
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.labels = level_order,
            panel.label.size = 1,
            legend.text.size = 0.8, 
            legend.width = 15, 
            frame = "black",
            legend.show=TRUE,
            frame.lwd = 0,
            panel.label.bg.color = NA)


# ##################################################################################
# ###### Animation######
# path <- 'E:\\Chapter2\\results\\edge'
# file_name = paste(path, 'edge_segfin_summary.csv',sep = '/')
# edge_segfin <- fread(file_name)
# edge_segfin <- data.frame(edge_segfin)
# 
# edge_segfin <- edge_segfin %>% filter(p == 1 & rem == 16)
# 
# N_abund <- aggregate(count ~  primary + segment, 
#                          data = as.data.frame(edge_segfin), 
#                          FUN = mean)
# 
# colnames(N_abund)[1:3] <- c("Month", "Segment", "Abundance")
#  
# jdr_abund <- merge(jdr_20k_sf, N_abund, by  = "Segment")
#  
# breaks = c(0.00001, 1, 2, 4, 6, 8, 10, 50) * 100000
# 
# pal <- c("grey", "lightblue", "gold", "darkorange", "red", "brown4", "black" )
# 
# # Create a set of separate maps for each month
# jdr_animation <- tmap_options(bg.color = 'white', legend.text.color = 'black') +
#   tm_shape(jdr_abund) +
#   tm_lines(
#     col = "Abundance",
#     palette = pal, 
#     breaks = breaks,
#     lwd = 5
#   ) +
#   tm_facets(along = "Month") + # along = "year" instead of by = "year"
#   tm_layout(legend.position = c("right", "top"), legend.text.size = 1,
#             legend.width = 2)
# 
# # Save the animated map as a gif file
# tmap_animation(
#   jdr_animation , filename = "jdr_abund.gif",
#   delay = 20)
# 
