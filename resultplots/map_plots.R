library(tidyverse)
library(here)
library(plyr)
library(data.table)
library(scales)
library(RColorBrewer) 
library(sf)
library(tmap)
library(RColorBrewer)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_segfin.csv',sep = '/')
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

nc <- all_segfin %>% filter(location == 'nocontrol')
all_segfin <- all_segfin %>%  filter(p == 1 & rem == 16)

all_segfin <- rbind(all_segfin, nc)
all_segfin <- all_segfin %>% select(segment, count, location)
colnames(all_segfin)[1] <- "Segment"
colnames(all_segfin)[2] <- "Crayfish abundance"

all_segfin$location[all_segfin$location == 'abund'] <- "Abundance"
all_segfin$location[all_segfin$location == 'down'] <- "Downstream"
all_segfin$location[all_segfin$location == 'edge'] <- "Edge"
all_segfin$location[all_segfin$location == 'grow'] <- "Growth"
all_segfin$location[all_segfin$location == 'random'] <- "Random"
all_segfin$location[all_segfin$location == 'nocontrol'] <- "No control"

jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))
jdr_abund <- merge(jdr_20k_sf, all_segfin, by  = "Segment")


breaks = c(0.00001, 1, 2, 4, 6, 8, 10) * 100000
colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )
color <- RColorBrewer::brewer.pal(11, "Paired")[1]


tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_abund) +
  tm_lines(
    col = "Crayfish abundance",
    palette = colors,
    breaks = breaks, 
    lwd = 5
  ) + 
  tm_facets(by=c("location"))+
  tm_layout(legend.position = c("right", "center"), 
            panel.label.size = 1.2,
            legend.text.size = 0.9, 
            legend.width = 2, 
            frame = "white",
            frame.lwd = 0,
            panel.label.bg.color = "white")


##################################################################################
###### Animation######
path <- 'D:\\Chapter2\\results\\edge'
file_name = paste(path, 'edge_segfin_summary.csv',sep = '/')
edge_segfin <- fread(file_name)
edge_segfin <- data.frame(edge_segfin)

edge_segfin <- edge_segfin %>% filter(p == 1 & rem == 16)

N_abund <- aggregate(count ~  primary + segment, 
                         data = as.data.frame(edge_segfin), 
                         FUN = mean)

colnames(N_abund)[1:3] <- c("Month", "Segment", "Abundance")
 
jdr_abund <- merge(jdr_20k_sf, N_abund, by  = "Segment")
 
breaks = c(0.00001, 1, 2, 4, 6, 8, 10, 50) * 100000

pal <- c("grey", "lightblue", "gold", "darkorange", "red", "brown4", "black" )

# Create a set of separate maps for each month
jdr_animation <- tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_abund) +
  tm_lines(
    col = "Abundance",
    palette = pal, 
    breaks = breaks,
    lwd = 5
  ) +
  tm_facets(along = "Month") + # along = "year" instead of by = "year"
  tm_layout(legend.position = c("right", "top"), legend.text.size = 1,
            legend.width = 2)

# Save the animated map as a gif file
tmap_animation(
  jdr_animation , filename = "jdr_abund.gif",
  delay = 20)

