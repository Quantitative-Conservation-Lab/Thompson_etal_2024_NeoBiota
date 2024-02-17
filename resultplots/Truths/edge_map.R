#Comparing the decision scenarios: 
library(tidyverse)
library(here)
library(abind)
library(plyr)
library(kableExtra)
library(gganimate)
library(gridExtra)
library(MCMCvis)
library(latex2exp)
library(coda)
library(scales)
library(ggthemes)
library(raster)
library(terra) 
library(spData) 
library(readr) 
library(readxl) 
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(maps)
library(spData)
library(magick)
library(grid)
library(viridis)
library(RColorBrewer)

I <- 35

##### edge Data #####
path <- here::here("results", "Truth_alternatives")

####True data summed across segments :####  
path2 <- "edge_truth"
file_name = paste(path, path2,'DR_edge_N_all.csv',sep = '/')
N_allsegssims_edge1 <- fread(file_name)
N_allsegssims_edge1 <- data.frame(N_allsegssims_edge1)[-1]

#Dafter
file_name = paste(path, path2,'DR_edge_D_all.csv',sep = '/')
Dafter_allsegssims_edge1 <- fread(file_name)
Dafter_allsegssims_edge1 <- data.frame(Dafter_allsegssims_edge1)[-1]

#Initial pop: 0
Ninit_allsegssims_edge1 <- filter(N_allsegssims_edge1, primary == 1)
Ninit_allsegssims_edge1$primary <- 0

#remove age = 0
Ninit_allsegssims_edge1$age <- Ninit_allsegssims_edge1$age -1
Ninit_allsegssims_edge1 <- filter(Ninit_allsegssims_edge1, age > 0)

Dafter_allsegssims_edge1$age <- Dafter_allsegssims_edge1$age -1
Dafter_allsegssims_edge1 <- filter(Dafter_allsegssims_edge1, age > 0)

#Combine init with Dafter
Dafter_mean_allseg_edge1 <- rbind(Ninit_allsegssims_edge1, Dafter_allsegssims_edge1)

N_allsims <- Dafter_mean_allseg_edge1

N_edge1 <- aggregate(count ~ primary + param + sim + segment, 
                         data = as.data.frame(N_allsims),
                         FUN = sum)



N_edge2 <- aggregate(count ~ primary + param + segment, 
                         data = as.data.frame(N_edge1),
                         FUN = mean)

N_edge <- aggregate(count ~ primary + segment, 
                          data = as.data.frame(N_edge2),
                          FUN = mean)

head(N_edge)


###### Leaflet example ######
#Plot final pop on map
jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))

finalN_edge <- N_edge %>% filter(primary == max(primary))
finalN_edge <- finalN_edge[-1]
colnames(finalN_edge)[1:2] <- c('Segment', 'Final')

jdr_edge <- merge(jdr_20k_sf, finalN_edge, by  = "Segment")

initN_edge <- N_edge %>% filter(primary == 0)
initN_edge <- initN_edge[-1]
colnames(initN_edge)[1:2] <- c('Segment', 'Initial')

jdr_edge <- merge(jdr_edge, initN_edge, by = "Segment")

jdr_edge <- st_transform(jdr_edge, "WGS84")

#Custom color palettes that depends on final pop
pal <- colorBin("YlOrRd", domain = jdr_edge$Final, bins = 6)

colors <- c("black", "#313695", "#74ADD1","gold", "darkorange", "red", "red4" )
pal <- colorBin(colors, domain = jdr_edge$Final, bins = 6)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% # Add a nice, neutral background
  #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addPolygons(data = jdr_edge, # Add polygons 
              color = ~pal(jdr_edge$Final), # Color the segment polygons
              weight = 2, # Increase polygon outline
              opacity = 1, # Make the polygon outline not see-through at all  
              dashArray = "3", # Make the polygon outlines dashed 
              fillOpacity = 0.7, # Make the polygons somewhat see-through
              highlightOptions = highlightOptions( # Defines pop-up labels  
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE), 
              label = paste("<strong>Segment</strong>: ", # Make this part of the label bolded
                            jdr_edge$Segment, "<br>",
                            "<strong>Fork</strong>: ", # Make this part of the label bolded
                            jdr_edge$Fork, "<br>",
                            "<strong>Initial Abundance</strong>: ", # Make this part of the label bolded
                            jdr_edge$Initial, "<br>",
                            "<strong>Final Abundance</strong>: ", # Make this part of the label bolded 
                            round(jdr_edge$Final,2)) %>% 
                lapply(htmltools::HTML), # We need this bit to properly apply the code creating bold words and line breaks
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(data = jdr_edge,
            pal = pal,
            values = ~Final, 
            opacity = 0.7, 
            title = "Final Abundance",
            position = "bottomright")

colnames(jdr_edge)[5] <- "Final Crayfish Abundance (per segment)"

breaks = c(0.00001, 1, 2, 4, 6, 8, 10) * 100000

colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )

edge_colors <- RColorBrewer::brewer.pal(11, "Paired")[3]

tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_edge) +
  tm_lines(
    col = "Final Crayfish Abundance (per segment)",
    palette = colors, #"YlOrRd",
    breaks = breaks, 
    border.col = "black",
    lty = "solid",
    lwd = 5
  ) + # along = "year" instead of by = "year"
  tm_layout(legend.position = c("right", "top"), legend.text.size = 1, 
            legend.width = 2, frame = edge_colors,frame.lwd = 12)


jdr_edge_polygons <- st_polygonize(jdr_edge)
jdr_edge_polygons <- as(jdr_edge_polygons, "Spatial") # If you want sp

tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_edge_polygons) +
  tm_polygons(
    col = "Final Crayfish Abundance (per segment)",
    palette = colors, #"YlOrRd",
    breaks = breaks, 
    border.col = "black",
    lty = "solid",
    lwd = 5
  ) + # along = "year" instead of by = "year"
  tm_layout(legend.position = c("right", "top"), legend.text.size = 1, 
            legend.width = 2, frame = edge_colors,frame.lwd = 12)


##################################################################################
###### Animation######

colnames(N_edge)[1:3] <- c("Month", "Segment", "Abundance")

jdr_edge <- merge(jdr_20k_sf, N_edge, by  = "Segment")

breaks = c(0, 1, 100, 500, 1000, 6000) * 1000

pal <-  brewer.pal(n = 11, name = "RdYlBu")
pal <- c("#313695", "#74ADD1", "#F46D43", "#A50026", "black" )
# Create a set of separate maps for each month
jdr_animation <- tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_edge) +
  tm_lines(
    col = "Abundance",
    palette = pal, #"YlOrRd",
    breaks = breaks
  ) +
  tm_facets(along = "Month") + # along = "year" instead of by = "year"
  tm_layout(legend.position = c("right", "top"), legend.text.size = 1)

# Save the animated map as a gif file
tmap_animation(
  jdr_animation , filename = "jdr_edge.gif",
  delay = 20)

