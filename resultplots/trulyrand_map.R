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

path <- here::here("results")

####True data summed across segments :####  
path2 <- "trulyrandom"
file_name = paste(path, path2,'DR_rand_N_all.csv',sep = '/')
N_allsegssims_rand1 <- fread(file_name)
N_allsegssims_rand1 <- data.frame(N_allsegssims_rand1)[-1]

#Dafter
file_name = paste(path, path2,'DR_rand_D_all.csv',sep = '/')
Dafter_allsegssims_rand1 <- fread(file_name)
Dafter_allsegssims_rand1 <- data.frame(Dafter_allsegssims_rand1)[-1]

#Initial pop: 0
Ninit_allsegssims_rand1 <- filter(N_allsegssims_rand1, primary == 1)
Ninit_allsegssims_rand1$primary <- 0

#remove age = 0
Ninit_allsegssims_rand1$age <- Ninit_allsegssims_rand1$age -1
Ninit_allsegssims_rand1 <- filter(Ninit_allsegssims_rand1, age > 0)

Dafter_allsegssims_rand1$age <- Dafter_allsegssims_rand1$age -1
Dafter_allsegssims_rand1 <- filter(Dafter_allsegssims_rand1, age > 0)

#Combine init with Dafter
Dafter_mean_allseg_rand1 <- rbind(Ninit_allsegssims_rand1, Dafter_allsegssims_rand1)

N_allsims <- Dafter_mean_allseg_rand1

N_rand1 <- aggregate(count ~ primary + param + sim + segment, 
                         data = as.data.frame(N_allsims),
                         FUN = sum)



N_rand2 <- aggregate(count ~ primary + param + segment, 
                         data = as.data.frame(N_rand1),
                         FUN = mean)

N_rand <- aggregate(count ~ primary + segment, 
                          data = as.data.frame(N_rand2),
                          FUN = mean)

head(N_rand)


###### Leaflet example ######
#Plot final pop on map
jdr_20k_sf <- read_sf(here::here("data", "initial_population", "JDR_20km_initpop.shp"))

finalN_rand <- N_rand %>% filter(primary == max(primary))
finalN_rand <- finalN_rand[-1]
colnames(finalN_rand)[1:2] <- c('Segment', 'Final')

jdr_rand <- merge(jdr_20k_sf, finalN_rand, by  = "Segment")

initN_rand <- N_rand %>% filter(primary == 0)
initN_rand <- initN_rand[-1]
colnames(initN_rand)[1:2] <- c('Segment', 'Initial')

jdr_rand <- merge(jdr_rand, initN_rand, by = "Segment")

jdr_rand <- st_transform(jdr_rand, "WGS84")

#Custom color palettes that depends on final pop
pal <- colorBin("YlOrRd", domain = jdr_rand$Final, bins = 6)

colors <- c("black", "#313695", "#74ADD1","gold", "darkorange", "red", "red4" )
pal <- colorBin(colors, domain = jdr_rand$Final, bins = 6)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% # Add a nice, neutral background
  #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addPolygons(data = jdr_rand, # Add polygons 
              color = ~pal(jdr_rand$Final), # Color the segment polygons
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
                            jdr_rand$Segment, "<br>",
                            "<strong>Fork</strong>: ", # Make this part of the label bolded
                            jdr_rand$Fork, "<br>",
                            "<strong>Initial Abundance</strong>: ", # Make this part of the label bolded
                            jdr_rand$Initial, "<br>",
                            "<strong>Final Abundance</strong>: ", # Make this part of the label bolded 
                            round(jdr_rand$Final,2)) %>% 
                lapply(htmltools::HTML), # We need this bit to properly apply the code creating bold words and line breaks
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(data = jdr_rand,
            pal = pal,
            values = ~Final, 
            opacity = 0.7, 
            title = "Final Abundance",
            position = "bottomright")

colnames(jdr_rand)[5] <- "Final Crayfish Abundance (per segment)"

breaks = c(0.00001, 1, 2, 4, 6, 8, 10) * 100000

colors <- c("grey", "lightblue", "gold", "darkorange", "red", "black" )

color <- RColorBrewer::brewer.pal(11, "Paired")[8]


tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_rand) +
  tm_lines(
    col = "Final Crayfish Abundance (per segment)",
    palette = colors, #"YlOrRd",
    breaks = breaks, 
    lwd = 5
  ) + # along = "year" instead of by = "year"
  tm_layout(legend.position = c("right", "top"), legend.text.size = 1, 
            legend.width = 2, frame = "gray",frame.lwd = 12)


##################################################################################
###### Animation######

colnames(N_rand)[1:3] <- c("Month", "Segment", "Abundance")

jdr_rand <- merge(jdr_20k_sf, N_rand, by  = "Segment")

breaks = c(0, 1, 100, 500, 1000, 6000) * 1000

pal <-  brewer.pal(n = 11, name = "RdYlBu")
pal <- c("#313695", "#74ADD1", "#F46D43", "#A50026", "black" )
# Create a set of separate maps for each month
jdr_animation <- tmap_options(bg.color = 'white', legend.text.color = 'black') +
  tm_shape(jdr_rand) +
  tm_lines(
    col = "Abundance",
    palette = pal, #"YlOrRd",
    breaks = breaks
  ) +
  tm_facets(along = "Month") + # along = "year" instead of by = "year"
  tm_layout(legend.position = c("right", "top"), legend.text.size = 1)

# Save the animated map as a gif file
tmap_animation(
  jdr_animation , filename = "jdr_rand.gif",
  delay = 20)

